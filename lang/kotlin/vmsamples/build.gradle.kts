plugins {
    alias(libs.plugins.kotlin.jvm)
}

dependencies {
    testImplementation(kotlin("test"))

    implementation(libs.graalvm.js)
    implementation(libs.graalvm.polyglot)
}