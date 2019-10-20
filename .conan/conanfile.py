from conans import ConanFile, CMake, tools
import ptvsd

# ptvsd.enable_attach()
# ptvsd.wait_for_attach()

class NanorangeConan(ConanFile):
    name = "NanoRange"
    version = "0.1.0"
    license = "BSL-1.0"
    author = "Tristan Brindle (tcbrindle at gmail dot com)"
    url = "https://github.com/tcbrindle/NanoRange"
    description = "Range-based goodness for C++17"
    exports = ("../*")
    # exports_sources = ("CMakeLists.txt", "single_include/*", "cmake/nanorangeConfig.cmake.in")
    generators = "cmake"

    def package(self):
        cmake = CMake(self)
        cmake.definitions["BUILD_TESTING"] = "OFF"
        cmake.definitions["USE_SINGLE_HEADER"] = "TRUE"
        cmake.definitions["DO_INSTALL"] = "TRUE"
        cmake.configure(build_folder='build')
        cmake.install()

    def package_id(self):
        self.info.header_only()

