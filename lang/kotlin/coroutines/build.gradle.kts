plugins {
    alias(libs.plugins.kotlin.jvm)
}

dependencies {
    testImplementation(kotlin("test"))

    implementation(libs.kotlinx.coroutines)
}
