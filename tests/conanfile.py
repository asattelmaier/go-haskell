from conans.model.conan_file import ConanFile
from conans import CMake
import os



current_path = os.getcwd()



class GoHaskell:
    app_name = "go-haskell"
    build_command = "cabal build"
    run_command = "-m json"

    @staticmethod
    def get_path():
        return os.path.normpath(current_path + 2 * (os.pardir + os.sep))

    @staticmethod
    def get_app_path():
        return "{0}{1}dist{1}build{1}go-haskell".format(GoHaskell.get_path(), os.sep)



class GoHaskellJsonApiTest(ConanFile):
    settings = "os", "compiler", "arch", "build_type"
    generators = "cmake"
    requires = ["gtest/1.10.0", "nlohmann_json/3.9.1"]
    test_app_name = "go_haskell_json_api_test"
    environment_variables = {
        "EXECUTE_GO_HASKELL_COMMAND": "\"cd %s && .%s%s %s \"" % (
            GoHaskell.get_app_path(),
            os.sep,
            GoHaskell.app_name,
            GoHaskell.run_command
        ),
    }

    def build(self):
        self.build_go_haskell()

        cmake = CMake(self)
        cmake.configure()
        cmake.build()
    
    def build_go_haskell(self):
        self.output.info("Build Go Haskell")
        self.run("cd %s && %s" % (GoHaskell.get_path(), GoHaskell.build_command))
        self.output.success("Build complete")

    def imports(self):
        self.copy("*.so", "bin", "lib")
        self.copy("*.dll", "bin", "bin")
        self.copy("*.dylib", "bin", "lib")

    def test(self):
        command = "cd bin && %s.%s%s" % (self.get_formatted_environment_variables(), os.sep, self.test_app_name)
        self.run(command)

    def get_formatted_environment_variables(self):
        keys = self.environment_variables.keys()
        values = self.environment_variables.values()

        return ''.join(["%s=%s " % (key, value) for key, value in zip(keys, values)])

