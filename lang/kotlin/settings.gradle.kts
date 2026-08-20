rootProject.name = "hobbies-kotlin"

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)

    repositories {
        mavenCentral()
    }
}

include("ai-agent")
include("ai")
include("codegen")
include("diff")
include("js")
include("sqlite")
include("stdlib")
include("graaljs")
include("coroutines")