plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.shadow)
}


dependencies {
    testImplementation(kotlin("test"))

    implementation(libs.protobuf)
    implementation(libs.kotlinpoet)
}

tasks {
    build {
        dependsOn(shadowJar)
    }
}