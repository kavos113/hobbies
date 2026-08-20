plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.flyway)
}

dependencies {
    testImplementation(kotlin("test"))

    implementation(libs.sqlite)
    implementation(libs.flyway.core)
}

flyway {
    url = "jdbc:sqlite:sample.db"
    user = ""
    password = ""
    locations = arrayOf("classpath:db/migration")
}