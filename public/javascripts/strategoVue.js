var strategoCols = [0,1,2,3,4,5,6,7,8,9]
var strategoRows = [0,1,2,3,4,5,6,7,8,9]

$(document).ready(function () {
    var strategoGame = new Vue({
        el:'#stratego-game'
    })
})

Vue.component('stratego-field', {
    template:`
        <div class="gameboard__container" id="board">
            <table>
                <tr v-for="r in rows">
                    <td class="cell" v-for="c in cols" v-bind:id="'row' + r + 'col' + c"><span><img v-bind:class="'piece ' + 'row' + r + 'col' + c" alt="" src="" /></span></td>
                </tr>
            </table>
        </div>
    `,
    data: function () {
        return {
            cols: strategoCols,
            rows: strategoRows
        }
    },
});

Vue.component('stratego-info', {
    template:`
        <div class="info__container">
            <p class="info__turn" id="infoPlayer"></p>
        </div>
    `,
});
