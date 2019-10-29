from conans import ConanFile, CMake, tools
import ptvsd

def get_version():
    git = tools.Git()
    try:
        return git.run("rev-parse --short=8 HEAD")
    except:
        return None

class NanorangeConan(ConanFile):
    name = "nanorange"
    version = get_version()
    license = "BSL-1.0"
    author = "Tristan Brindle (tcbrindle at gmail dot com)"
    url = "https://github.com/tcbrindle/NanoRange"
    description = "Range-based goodness for C++17"
    exports_sources = ("CMakeLists.txt", "single_include/*", "cmake/nanorangeConfig.cmake.in", "LICENSE_1_0.txt")
    generators = "cmake"

    def package(self):
        ptvsd.break_into_debugger()
        self.copy("", "", "")
        cmake = CMake(self)
        cmake.definitions["BUILD_TESTING"] = "OFF"
        cmake.definitions["USE_SINGLE_HEADER"] = "TRUE"
        cmake.definitions["DO_INSTALL"] = "TRUE"
        cmake.configure(build_folder='build')
        cmake.install()

    def package_id(self):
        self.info.header_only()

