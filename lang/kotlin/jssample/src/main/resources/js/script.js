function greet(name) {
    return "Hello, " + name + " from JavaScript!";
}

function setOutput(message) {
    document.getElementById("output").textContent = message;
}

const jsObject = {
    message: "Message from JS Object",
    log: function(text) {
        console.log("JS Log:", text);
    }
};

class JsCounter {
    constructor(initialValue) {
        this.count = initialValue || 0;
    }
    
    increment() {
        this.count++;
    }
    
    getCount() {
        return this.count;
    }
}