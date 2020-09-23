// R Console

const RConsole = {};

RConsole.getConsole = function(id) {
    return document.querySelector(`#${id}`);
};

RConsole.update = function(id, text) {
    const previousText = this.getConsole(id).textContent;
    this.getConsole(id).textContent = previousText + text;
    this.autoScrollDown(id);
};

RConsole.clear = function(id) {
    this.getConsole(id).textContent = '';
};

RConsole.autoScrollDown = function(id) {
    this.getConsole(id).scrollTop = this.getConsole(id).scrollHeight;
};

RConsole.eventHandler = function(message) {
    const {id, action, text} = message;
    if (action === 'update') {
        this.update(id, text);
    } else if (action === 'clear') {
        this.clear(id);
    }
};


// Shiny Events

Shiny.addCustomMessageHandler('rconsole', RConsole.eventHandler.bind(RConsole));
