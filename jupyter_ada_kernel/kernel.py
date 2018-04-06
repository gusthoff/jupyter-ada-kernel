from queue import Queue
from threading import Thread

from ipykernel.kernelbase import Kernel
import re
import subprocess
import os
import os.path as path

class RealTimeSubprocess(subprocess.Popen):
    """
    A subprocess that allows to read its stdout and stderr in real time
    """

    def __init__(self, cmd, write_to_stdout, write_to_stderr):
        """
        :param cmd: the command to execute
        :param write_to_stdout: a callable that will be called with chunks of data from stdout
        :param write_to_stderr: a callable that will be called with chunks of data from stderr
        """
        self._write_to_stdout = write_to_stdout
        self._write_to_stderr = write_to_stderr

        super().__init__(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)

        self._stdout_queue = Queue()
        self._stdout_thread = Thread(target=RealTimeSubprocess._enqueue_output, args=(self.stdout, self._stdout_queue))
        self._stdout_thread.daemon = True
        self._stdout_thread.start()

        self._stderr_queue = Queue()
        self._stderr_thread = Thread(target=RealTimeSubprocess._enqueue_output, args=(self.stderr, self._stderr_queue))
        self._stderr_thread.daemon = True
        self._stderr_thread.start()

    @staticmethod
    def _enqueue_output(stream, queue):
        """
        Add chunks of data from a stream to a queue until the stream is empty.
        """
        for line in iter(lambda: stream.read(4096), b''):
            queue.put(line)
        stream.close()

    def write_contents(self):
        """
        Write the available content from stdin and stderr where specified when the instance was created
        :return:
        """

        def read_all_from_queue(queue):
            res = b''
            size = queue.qsize()
            while size != 0:
                res += queue.get_nowait()
                size -= 1
            return res

        stdout_contents = read_all_from_queue(self._stdout_queue)
        if stdout_contents:
            self._write_to_stdout(stdout_contents)
        stderr_contents = read_all_from_queue(self._stderr_queue)
        if stderr_contents:
            self._write_to_stderr(stderr_contents)


class AdaKernel(Kernel):
    implementation = 'jupyter_ada_kernel'
    implementation_version = '0.1'
    language = 'ada'
    language_version = '2012'
    language_info = {'name': 'ada',
                     'mimetype': 'text/x-ada',
                     'file_extension': '.ada'}
    banner = "Ada kernel.\n" \
             "Uses gcc and creates source code files and executables in temporary folder.\n"
    output_format = "raw"

    def __init__(self, *args, **kwargs):
        super(AdaKernel, self).__init__(*args, **kwargs)
        self.files = []

    def cleanup_files(self):
        """Remove all the temporary files created by the kernel"""
        for file in self.files:
            if os.path.exists(file):
                os.remove(file)

    def new_src_file(self, filename):
        """Create a new temp file to be deleted when the kernel shuts down"""
        # We don't want the file to be deleted when closed, but only when the kernel stops
        if os.path.exists(filename):
            os.remove(filename)
        file = open(filename, 'w')
        self.files.append(filename)
        return file

    def _write_to_stdout(self, contents):
        if self.output_format == "raw":
            self.send_response(self.iopub_socket, 'stream', {'name': 'stdout', 'text': contents})
        elif self.output_format == "json":
            import json
            self.send_response(self.iopub_socket, 'display_data', json.loads(contents));
        else:
            self.send_response(self.iopub_socket, 'display_data',
                { 'data': { self.output_format : contents },
                'metadata': {}
                })

    def _write_to_stderr(self, contents):
        self.send_response(self.iopub_socket, 'stream', {'name': 'stderr', 'text': contents})

    def create_jupyter_subprocess(self, cmd):
        return RealTimeSubprocess(cmd,
                                  lambda contents: self._write_to_stdout(contents.decode()),
                                  lambda contents: self._write_to_stderr(contents.decode()))

    def compile_with_gcc(self, source_filename, cflags=None):
        """Compile code using gnat/gcc"""
        args = ['gnat', "compile", "-gnatc", source_filename]
        if cflags is not None:
            args.extend(cflags)
        return self.create_jupyter_subprocess(args)

    def build_with_gcc(self, source_filename, make_flags=None):
        """Build code using gnat/gcc"""
        args = ['gnatmake', '-f', source_filename]
        if make_flags is not None:
            args.extend(make_flags)
        return self.create_jupyter_subprocess(args)

    def _filter_magics(self, code):

        magics = {'cflags': [],
                  'make_flags': [],
                  'args': []}

        for line in code.splitlines():
            if line.startswith('--%'):
                key, value = line[3:].split(":", 2)
                key = key.strip().lower()
                value = value.lstrip()

                if key in ['make_flags', 'cflags']:
                    for flag in value.split(" "):
                        magics[key] += [flag]
                elif key == "args":
                    # Split arguments respecting quotes
                    for argument in re.findall(r'(?:[^\s,"]|"(?:\\.|[^"])*")+', value):
                        magics['args'] += [argument.strip('"')]
                elif key == "filename":
                    magics[key] = value.lower()
                elif key == "run":
                    magics[key] = value.lower()
                elif key == "run_file":
                    magics[key] = value.lower()
                elif key == "output":
                    self.output_format = value.lower()

        return magics

    def do_execute(self, code, silent, store_history=True,
                   user_expressions=None, allow_stdin=False):

        magics = []
        try:
            magics = self._filter_magics(code)
        except:
            print("Cannot retrieve magic string")

        binary_filename = None
        source_filename = None

        if "run_file" in magics:
            source_filename = magics['run_file']
            binary_filename = os.path.splitext(magics['run_file'])[0]

        if "filename" in magics:
            source_filename = magics['filename']

        if "run" in magics and not "run_file" in magics:
            binary_filename = os.path.splitext(magics['run'])[0]
            if source_filename is None:
                source_filename = binary_filename + ".adb"

        if source_filename is not None:
            with self.new_src_file(source_filename) as source_file:
                source_file.write(code)
                source_file.flush()

            p = self.compile_with_gcc(source_filename, magics['cflags'])
            while p.poll() is None:
                p.write_contents()
            p.write_contents()

        if binary_filename is not None:
            p = self.build_with_gcc(source_filename, magics['make_flags'])

            while p.poll() is None:
                p.write_contents()
            p.write_contents()
            if p.returncode != 0:  # Compilation failed
                self._write_to_stderr(
                        "[Ada kernel] GNAT exited with code {}, the executable will not be executed".format(
                                p.returncode))
                return {'status': 'ok', 'execution_count': self.execution_count, 'payload': [],
                        'user_expressions': {}}


            p = self.create_jupyter_subprocess(["./" + binary_filename] + magics['args'])
            while p.poll() is None:
                p.write_contents()
            p.write_contents()

            if p.returncode != 0:
                self._write_to_stderr("[Ada kernel] Executable exited with code {}".format(p.returncode))

        return {'status': 'ok', 'execution_count': self.execution_count, 'payload': [], 'user_expressions': {}}

    def do_shutdown(self, restart):
        """Cleanup the created source code files and executables when shutting down the kernel"""
        self.cleanup_files()
