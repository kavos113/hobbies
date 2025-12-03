plugins {
  kotlin("jvm") version "2.1.20"
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
  mavenCentral()
}

dependencies {
  testImplementation(kotlin("test"))

  implementation("com.openai:openai-java:1.4.1")
}

tasks.test {
  useJUnitPlatform()
}
kotlin {
  jvmToolchain(21)
}