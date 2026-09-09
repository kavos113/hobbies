package com.github.kavos113.trafficmonitor

import android.Manifest
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.result.contract.ActivityResultContracts
import androidx.core.content.ContextCompat

class MainActivity : ComponentActivity() {
  private val requestPermissionLauncher = registerForActivityResult(ActivityResultContracts.RequestPermission()) { isGranted ->
    if (isGranted) {
      startTrafficService()
    }
  }

  override fun onCreate(savedInstanceState: Bundle?) {
    super.onCreate(savedInstanceState)

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
      if (ContextCompat.checkSelfPermission(this, Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED) {
        startTrafficService()
      } else {
        requestPermissionLauncher.launch(Manifest.permission.POST_NOTIFICATIONS)
      }
    } else {
      startTrafficService()
    }
  }

  private fun startTrafficService() {
    val intent = Intent(this, TrafficSpeedService::class.java)
    ContextCompat.startForegroundService(this, intent)
  }
}