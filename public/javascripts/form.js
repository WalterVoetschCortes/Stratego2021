const form = document.getElementById('form');
const player1 = document.getElementById('inputPlayer1');
const player2 = document.getElementById('inputPlayer2');

$('#loadButton').click(function() {
    location.href='/load';
});

form.addEventListener('submit', e => {
    e.preventDefault();

    checkInputs();
});

function checkInputs() {
    //get values from inputs, trim to remove whitespaces
    //alert("checkInputs");
    const player1Value =  player1.value.trim();
    const player2Value = player2.value.trim();

    if(player1Value === ''){
        //show error
        //add error class
        setErrorFor(player1, 'Set name of Player 1!')
        if(player2Value === ''){
            setErrorForBoth(player1, player2, 'Names are not set!')
        }
        return;
    }else{
        setSuccessFor(player1)
        //add success class
    }
    if(player2Value === ''){
        //show error
        //add error class
        setErrorFor(player2, 'Set name of Player 2!')
        if(player1Value === ''){
            setErrorForBoth(player1, player2, 'Names are not set!')
        }
        return;
    }else {
        setSuccessFor(player2)
    }
    if(player1Value.toUpperCase() === player2Value.toUpperCase()){
        setErrorForBoth(player1, player2, 'Names cannot be the same!')
        return;
    }

    location.href='/setPlayers/' + player1Value + '/' + player2Value;

}

function setErrorFor(input, message) {
    const nameWrapper = input.parentElement; //.nameWrapper
    const small = nameWrapper.querySelector('small');

    // add error message inside small
    small.innerText = message;

    // add error class
    nameWrapper.className = 'nameWrapper error';
}

function setErrorForBoth(input1, input2, message) {
    const nameWrapper1 = input1.parentElement; //.nameWrapper
    const small1 = nameWrapper1.querySelector('small');
    const nameWrapper2 = input2.parentElement; //.nameWrapper
    const small2 = nameWrapper2.querySelector('small');

    small1.innerText = message;
    small2.innerText = message;

    nameWrapper1.className = 'nameWrapper error';
    nameWrapper2.className = 'nameWrapper error';
}

function setSuccessFor(input) {
    const nameWrapper = input.parentElement; //.nameWrapper
    nameWrapper.className = 'nameWrapper success';
}
