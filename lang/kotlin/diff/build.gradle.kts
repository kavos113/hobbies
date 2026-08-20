plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.serialization)
}

dependencies {
    testImplementation(kotlin("test"))

    implementation(libs.diffutils)
    implementation(libs.jgit)
    implementation(libs.anthropic)
    implementation(libs.jackson.core)
    implementation(libs.jackson.module.kotlin)
    implementation(libs.ktreesitter)
    implementation(libs.treesitter.java)
    implementation(libs.treesitter.ts)
}
