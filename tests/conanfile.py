from conans.model.conan_file import ConanFile
from conans import CMake
import os



current_path = os.getcwd()



class GoHaskell:
    app_name = "go-haskell"
    build_command = "cabal build"
    dist_dir = "dist-newstyle"

    @staticmethod
    def get_run_socket_server_command():
        execute_command = "sh -c \"chmod +x {} && {} socket &\"" 

        return "find {0} -name {1} -type f -exec {2} \;".format(
                GoHaskell.get_path() + os.sep + GoHaskell.dist_dir,
                GoHaskell.app_name,
                execute_command
            )


    @staticmethod
    def get_path():
        return os.path.normpath(current_path + 2 * (os.pardir + os.sep))



class TestGoHaskellSocketApi(ConanFile):
    settings = "os", "compiler", "arch", "build_type"
    generators = "cmake"
    requires = ["gtest/1.10.0", "nlohmann_json/3.9.1", "websocketpp/0.8.2"]
    test_app_name = "go_haskell_socket_api_test"
    environment_variables = {}

    def build(self):
        self.run_go_haskell_socket_server()

        cmake = CMake(self)
        cmake.configure()
        cmake.build()
    
    def run_go_haskell_socket_server(self):
        # TODO: Make Port configurableboard
        self.output.info("Run Go Haskell Socket Server")
        self.run(GoHaskell.get_run_socket_server_command())
        self.output.success("Socket Server is running")
    
    def imports(self):
        self.copy("*.so", "bin", "lib")
        self.copy("*.dll", "bin", "bin")
        self.copy("*.dylib", "bin", "lib")

    def test(self):
        # TODO: Pass arguments to gTest for filtering
        # test like --gtest_filter=Rule3*
        command = "cd bin && %s.%s%s" % (
            self.get_formatted_environment_variables(),
            os.sep,
            self.test_app_name
        )
        self.run(command)
        self.run("kill `pidof %s`" % GoHaskell.app_name)
    

    def get_formatted_environment_variables(self):
        keys = self.environment_variables.keys()
        values = self.environment_variables.values()

        return ''.join(
            ["%s=%s " % (key, value) for key, value in zip(keys, values)]
        )

