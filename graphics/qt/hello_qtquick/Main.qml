import QtQuick
import QtQuick.Window

Window {
    width: 640
    height: 480
    visible: true
    title: qsTr("Hello Qt Quick")

    Rectangle {
        anchors.fill: parent
        color: "#f0f0f0"

        Text {
            text: "Hello, World!"
            anchors.centerIn: parent
            font.pixelSize: 24
            color: "#333333"
        }
    }
}