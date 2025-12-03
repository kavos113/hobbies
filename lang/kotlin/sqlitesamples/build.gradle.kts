plugins {
    kotlin("jvm") version "2.1.10"
    id("org.flywaydb.flyway") version "11.4.0"
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.xerial:sqlite-jdbc:3.49.1.0")
    implementation("org.flywaydb:flyway-core:11.4.0")

    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}

flyway {
    url = "jdbc:sqlite:sample.db"
    user = ""
    password = ""
    locations = arrayOf("classpath:db/migration")
}