package org.example

import org.flywaydb.core.Flyway
import java.sql.Connection
import java.sql.DriverManager
import java.sql.SQLException
import java.sql.Statement

fun main() {
    try {
        val flyway = Flyway.configure()
            .dataSource("jdbc:sqlite:sample.db", "", "")
            .locations("classpath:db/migration")
            .load()

        flyway.migrate()
    } catch (e: SQLException) {
        println(e.message)
    }

    try {
        DriverManager.getConnection("jdbc:sqlite:sample.db").use { conn ->
            conn.createStatement().use { stmt ->
                val rs = stmt.executeQuery("SELECT * FROM hresults LIMIT 5")
                while (rs.next()) {
                    println(rs.getString("name"))
                }
            }
        }
    } catch (e: SQLException) {
        println(e.message)
    }
}