from ipykernel.kernelapp import IPKernelApp
from .kernel import AdaKernel
IPKernelApp.launch_instance(kernel_class=AdaKernel)
