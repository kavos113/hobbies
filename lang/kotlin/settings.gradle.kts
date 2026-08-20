rootProject.name = "hobbies-kotlin"

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)

    repositories {
        mavenCentral()
    }
}

include("ai-agent-test")
include("ai-test")
include("codegentest")
include("diff-test")
include("jssample")
include("sqlitesamples")
include("stdlib")
include("vmsamples")