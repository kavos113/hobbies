package com.github.kavos113.lualatexbuilder

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.process.ProcessAdapter
import com.intellij.execution.process.ProcessEvent
import com.intellij.notification.NotificationGroupManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.util.Key
import com.intellij.openapi.vfs.VfsUtil
import com.intellij.openapi.vfs.VirtualFileManager
import java.util.concurrent.CountDownLatch
import kotlin.io.path.Path

class BuildAction: AnAction() {
    
    override fun actionPerformed(p0: AnActionEvent) {
        val project = p0.project
        val editor = FileEditorManager.getInstance(project!!).selectedTextEditor
        val file = FileDocumentManager.getInstance().getFile(editor?.document ?: return) ?: return

        println(file.parent.path)
        println(file.name)

        build(file.parent.path, file.name)

        val pdfExisted = Path(file.parent.path, file.nameWithoutExtension + ".pdf").toFile().exists()
        if (pdfExisted) {
            NotificationGroupManager.getInstance()
                .getNotificationGroup("LuaLaTeX Builder")
                .createNotification("Success", "LuaLaTeX build successfully", NotificationType.INFORMATION)
                .notify(project)
        } else {
            NotificationGroupManager.getInstance()
                .getNotificationGroup("LuaLaTeX Builder")
                .createNotification("Error", "LuaLaTeX build failed", NotificationType.ERROR)
                .notify(project)
        }
    }

    fun build(path: String, name: String) {
        val stringBuilder = StringBuilder()

        val commandPartsBuild = listOf("lualatex", name)

        val commandLine = GeneralCommandLine(commandPartsBuild)
        commandLine.charset = Charsets.UTF_8
        commandLine.setWorkDirectory(path)

        val latch = CountDownLatch(1)

        val processHandler = OSProcessHandler(commandLine)
        processHandler.addProcessListener(object : ProcessAdapter() {
            override fun onTextAvailable(event: ProcessEvent, outputType: Key<*>) {
                stringBuilder.append(event.text)
//                println(event.text)
                if (event.text == "? ") {
//                    println("error")
                    latch.countDown()
                }
            }

            override fun processTerminated(event: ProcessEvent) {
                latch.countDown()
            }
        })

        processHandler.startNotify()
        latch.await()

        val virtualFile = VirtualFileManager.getInstance().findFileByNioPath(Path(path))
        virtualFile?.let {
            VfsUtil.markDirtyAndRefresh(true, true, true, it)
        }
    }
}