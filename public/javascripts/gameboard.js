
let size = 10
let colA = 0
let rowA = 0
let dir = "d"


class MatchField {

    constructor() {
        this.fields = []
        this.currentPlayerIndex = 0
    }

    updateView () {
        let num = 0
        for(let row = 0; row < size; row++) {
            for(let col = 0; col < size; col++) {
                if(this.fields[num].colour === 0) {
                    let td = document.getElementById("row"+ row + "col" + col)
                    let img = $("." + "row"+ row + "col" + col)
                    td.className = "cell cells_blue"
                    switch (this.fields[num].figName) {
                        case 'F':
                            img
                                .attr('src',"/assets/images/character-flag.svg")
                                .attr('alt',"F")
                            break;
                        case 'B':
                            img
                                .attr('src',"/assets/images/character-bomb.svg")
                                .attr('alt',"B")
                            break;
                        case 'M':
                            img
                                .attr('src',"/assets/images/character-marshal.svg")
                                .attr('alt',"M")
                            break;
                        case '1':
                            img
                                .attr('src',"/assets/images/character-spy.svg")
                                .attr('alt',"1")
                            break;
                        case '2':
                            img
                                .attr('src',"/assets/images/character-scout.svg")
                                .attr('alt',"2")
                            break;
                        case '3':
                            img
                                .attr('src',"/assets/images/character-miner.svg")
                                .attr('alt',"3")
                            break;
                        case '4':
                            img
                                .attr('src',"/assets/images/character-sergeant.svg")
                                .attr('alt',"4")
                            break;
                        case '5':
                            img
                                .attr('src',"/assets/images/character-lieutenant.svg")
                                .attr('alt',"5")
                            break;
                        case '6':
                            img
                                .attr('src',"/assets/images/character-captain.svg")
                                .attr('alt',"6")
                            break;
                        case '7':
                            img
                                .attr('src',"/assets/images/character-major.svg")
                                .attr('alt',"7")
                            break;
                        case "8":
                            img
                                .attr('src',"/assets/images/character-colonel.svg")
                                .attr('alt',"8")
                            break;
                        case "9":
                            img
                                .attr('src',"/assets/images/character-general.svg")
                                .attr('alt',"9")
                            break;
                    }
                } else if (this.fields[num].colour === 1) {
                    let td = document.getElementById("row"+ row + "col" + col)
                    let img = $("." + "row"+ row + "col" + col)
                    td.className = "cell cells_red"
                    switch (this.fields[num].figName) {
                        case 'F':
                            img
                                .attr('src',"/assets/images/character-flag.svg")
                                .attr('alt',"F")
                            break;
                        case 'B':
                            img
                                .attr('src',"/assets/images/character-bomb.svg")
                                .attr('alt',"B")
                            break;
                        case 'M':
                            img
                                .attr('src',"/assets/images/character-marshal.svg")
                                .attr('alt',"M")
                            break;
                        case '1':
                            img
                                .attr('src',"/assets/images/character-spy.svg")
                                .attr('alt',"1")
                            break;
                        case '2':
                            img
                                .attr('src',"/assets/images/character-scout.svg")
                                .attr('alt',"2")
                            break;
                        case '3':
                            img
                                .attr('src',"/assets/images/character-miner.svg")
                                .attr('alt',"3")
                            break;
                        case '4':
                            img
                                .attr('src',"/assets/images/character-sergeant.svg")
                                .attr('alt',"4")
                            break;
                        case '5':
                            img
                                .attr('src',"/assets/images/character-lieutenant.svg")
                                .attr('alt',"5")
                            break;
                        case '6':
                            img
                                .attr('src',"/assets/images/character-captain.svg")
                                .attr('alt',"6")
                            break;
                        case '7':
                            img
                                .attr('src',"/assets/images/character-major.svg")
                                .attr('alt',"7")
                            break;
                        case "8":
                            img
                                .attr('src',"/assets/images/character-colonel.svg")
                                .attr('alt',"8")
                            break;
                        case "9":
                            img
                                .attr('src',"/assets/images/character-general.svg")
                                .attr('alt',"9")
                            break;
                    }
                } else {
                    let td = document.getElementById("row"+ row + "col" + col)
                    td.className = "cell cells__green" //empty cell
                    $("." + "row"+ row + "col" + col)
                        .attr('src',"")
                        .attr('alt',"")
                }
                num++;
            }
        }
    }

    move(dir, row, col) {
        $.ajax({
            method: "POST",
            url: "/move",
            dataType: "json",
            contentType: "application/json",
            data: JSON.stringify({
                "row": row,
                "col": col,
                "dir": dir
            }),

            success: (result) => {
                const {matchField, currentPlayer, currentPlayerIndex} = result
                this.updateMatchField(matchField)
                this.updateView()
                this.updateCurrentPlayer(currentPlayer, currentPlayerIndex)
            }
        });
    }

    updateMatchField(newFields) {
        this.fields = newFields
    }

    updateCurrentPlayer(currentPlayer, currentPlayerIndex){
        this.currentPlayerIndex = currentPlayerIndex
        document.getElementById("infoPlayer").innerHTML = currentPlayer + ", it's your turn!"
    }
}

$(document).on('click', '.cells_blue',(function () {
    colA = this.parentElement.rowIndex
    rowA = this.cellIndex

    // changes background color of selected cell:
    if(matchField.currentPlayerIndex === 0){
        $(".cell").removeClass('selectedCell');
        $(this).addClass('selectedCell');
    }
}))

$(document).on('click', '.cells_red',(function () {
    colA = this.parentElement.rowIndex
    rowA = this.cellIndex

    // changes background color of selected cell:
    if(matchField.currentPlayerIndex === 1){
        $(".cell").removeClass('selectedCell');
        $(this).addClass('selectedCell');
    }
}))

$(document).keydown(function(event){
    var key = event.which;
    switch(key) {
        case 37:
            dir = "l"
            console.log("left")
            // Key left.
            break;
        case 38:
            dir = "u"
            console.log("up")
            // Key up.
            break;
        case 39:
            dir = "r"
            console.log("right")
            // Key right.
            break;
        case 40:
            dir = "d"
            console.log("down")
            // Key down.
            break;
        case 65:
            attacking = !attacking
    }
    matchField.move(dir, rowA, colA)
});

function loadJson() {
    $.ajax({
        method: "GET",
        url: "/json",
        dataType: "json",

        success: function (result) {
            matchField = new MatchField();
            matchField.updateMatchField(result.matchField);
            matchField.updateView();
            matchField.updateCurrentPlayer(result.currentPlayer, result.currentPlayerIndex)
        }
    });
}

function connectWebSocket(){
    var webSocket = new WebSocket("ws://localhost:9000/websocket");

    webSocket.onopen = () => {
        console.log("Connected to WebSocket")
    };

    webSocket.onclose = () => function () {
        console.log('Connection with Websocket Closed!');
    };

    webSocket.onerror = function (error) {
        console.log('Error in Websocket Occured: ' + error);
    };

    webSocket.onmessage = function (e) {
        console.log("message");
        if (typeof e.data === "string") {
            let json = JSON.parse(e.data);
            let fields = json.matchField;
            let currentPlayerIndex = json.currentPlayerIndex;
            let currentPlayer = json.currentPlayer;
            console.log("fields: " + fields)
            console.log("playerIndex: " + currentPlayerIndex)

            matchField.updateMatchField(fields);
            matchField.updateView();
            matchField.updateCurrentPlayer(currentPlayer, currentPlayerIndex)
        }
    }
}

$( document ).ready(function() {
    console.log( "Document is ready, filling matchfield" );
    loadJson();
    connectWebSocket();
});
