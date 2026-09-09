package com.github.kavos113.trafficmonitor

import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.Service
import android.content.Context
import android.content.Intent
import android.content.pm.ServiceInfo
import android.graphics.Bitmap
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.graphics.Typeface
import android.net.TrafficStats
import android.os.Build
import android.os.IBinder
import androidx.core.app.NotificationCompat
import androidx.core.graphics.createBitmap
import androidx.core.graphics.drawable.IconCompat
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch
import java.util.Locale
import kotlin.time.Duration.Companion.milliseconds

class TrafficSpeedService : Service() {

  private val scope = CoroutineScope(Dispatchers.Default + Job())
  private var lastRxBytes = 0L
  private var lastTxBytes = 0L
  private var lastTime = 0L

  companion object {
    private const val CHANNEL_ID = "traffic_speed_channel"
    private const val NOTIFICATION_ID = 1001
  }

  private fun formatSpeed(bps: Double): String = when {
    bps >= 1024 * 1024 -> String.format(Locale.US, "%.1fMbps", bps / (1024 * 1024))
    bps >= 1024 -> String.format(Locale.US, "%.0fkbps", bps / 1024)
    else -> String.format(Locale.US, "%.0fbps", bps)
  }

  private fun formatSpeedCompact(bps: Double): String = when {
    bps >= 1024 * 1024 -> String.format(Locale.US, "%.0fM", bps / (1024 * 1024))
    bps >= 1024 -> String.format(Locale.US, "%.0fk", bps / 1024)
    else -> "0k"
  }

  override fun onCreate() {
    super.onCreate()
    createNotificationChannel()

    lastRxBytes = TrafficStats.getTotalRxBytes()
    lastTxBytes = TrafficStats.getTotalTxBytes()
    lastTime = System.currentTimeMillis()

    val startNotification = NotificationCompat.Builder(this, CHANNEL_ID)
      .setContentTitle("0kbps")
      .setContentText("↓ 0kbps / ↑ 0kbps")
      .setOngoing(true)
      .setOnlyAlertOnce(true)
      .setPriority(NotificationCompat.PRIORITY_LOW)
      .setSmallIcon(android.R.drawable.stat_sys_download)
      .build()

    startForeground(NOTIFICATION_ID, startNotification)

//    println("started")

    startMonitoring()
  }

  private fun startMonitoring() {
    scope.launch {
      while (isActive) {
        delay(1000.milliseconds)

        val currentRxBytes = TrafficStats.getTotalRxBytes()
        val currentTxBytes = TrafficStats.getTotalTxBytes()
        val currentTime = System.currentTimeMillis()

        val timeDiffSec = (currentTime - lastTime) / 1000.0

        if (timeDiffSec > 0) {
          val rxSpeed = (currentRxBytes - lastRxBytes) / timeDiffSec
          val txSpeed = (currentTxBytes - lastTxBytes) / timeDiffSec

          lastRxBytes = currentRxBytes
          lastTxBytes = currentTxBytes
          lastTime = currentTime

          val dlbps = formatSpeed(rxSpeed * 8.0)
          val ulbps = formatSpeed(txSpeed * 8.0)

//          println("monitor: ↓ $dlbps bps / ↑ $ulbps bps")

          val iconText = formatSpeedCompact(rxSpeed * 8.0)
          val iconBitmap = createTextBitmap(iconText)

          val notification = buildNotification(
            title = "↓ $dlbps / ↑ $ulbps",
            iconBitmap = iconBitmap
          )

          val manager = getSystemService(NotificationManager::class.java)
          manager.notify(NOTIFICATION_ID, notification)
        }
      }
    }
  }

  private fun buildNotification(title: String, iconBitmap: Bitmap): Notification {
    return NotificationCompat.Builder(this, CHANNEL_ID)
      .setContentTitle(title)
      .setContentText(null)
      .setOngoing(true)
      .setOnlyAlertOnce(true)
      .setPriority(NotificationCompat.PRIORITY_LOW)
      .setSmallIcon(IconCompat.createWithBitmap(iconBitmap))
      .build()
  }

  private fun createTextBitmap(text: String): Bitmap {
    val size = 96
    val bitmap = createBitmap(size, size)
    val canvas = Canvas(bitmap)
    val paint = Paint().apply {
      color = Color.WHITE
      textSize = 52f
      isAntiAlias = true
      textAlign = Paint.Align.CENTER
      typeface = Typeface.create(Typeface.SANS_SERIF, Typeface.BOLD)
    }

    val yPos = (canvas.height / 2 - (paint.descent() + paint.ascent()) / 2)
    canvas.drawText(text, size / 2f, yPos, paint)

    return bitmap
  }

  private fun createNotificationChannel() {
    val channel = NotificationChannel(
      CHANNEL_ID,
      "Network Speed Monitor",
      NotificationManager.IMPORTANCE_DEFAULT
    ).apply {
      description = "display network speed"
      setShowBadge(false)
      setSound(null, null)
      enableVibration(false)
    }

    val manager = getSystemService(NotificationManager::class.java)
    manager.createNotificationChannel(channel)
  }

  override fun onBind(p0: Intent?): IBinder? = null

  override fun onDestroy() {
    scope.cancel()
    super.onDestroy()
  }
}