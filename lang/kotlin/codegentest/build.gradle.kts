plugins {
    kotlin("jvm") version "2.2.21"
    application
    
    id("com.gradleup.shadow") version "9.2.2"
}

group = "org.example"
version = "1.0-SNAPSHOT"

application {
    mainClass = "org.example.MainKt"
}

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    implementation("com.google.protobuf:protobuf-java:4.33.4")
    implementation("com.squareup:kotlinpoet:2.2.0")
}

kotlin {
    jvmToolchain(21)
}

tasks.test {
    useJUnitPlatform()
}

tasks {
    build {
        dependsOn(shadowJar)
    }
}