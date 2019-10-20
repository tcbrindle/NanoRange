import os

from conans import ConanFile, CMake, tools


class NanorangeTestConan(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "cmake"

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.build()

    def test(self):
        assert os.path.isfile(os.path.join(self.deps_cpp_info["nanorange"].rootpath, "licenses", "LICENSE_1_0.txt"))
        bin_path = os.path.join("bin", "test_package")
        self.run("%s -s" % bin_path, run_environment=True)
