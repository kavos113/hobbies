plugins {
    id("java")
}

group = "com.github.kavos113"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(platform("org.junit:junit-bom:6.0.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<JavaCompile> {
    val jniDir = file("src/main/cpp")

    doFirst {
        jniDir.mkdirs()
    }

    options.compilerArgs.addAll(
        listOf(
            "-h", jniDir.absolutePath
        )
    )
}