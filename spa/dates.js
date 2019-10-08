var getUnixTime = require('date-fns/getUnixTime')
var setISOWeek = require('date-fns/setISOWeek')
var startOfISOWeek = require('date-fns/startOfISOWeek')
var endOfISOWeek = require('date-fns/endOfISOWeek')
var getISOWeekYear = require('date-fns/getISOWeekYear')
var getISOWeek = require('date-fns/getISOWeek')

window.toWindow = function toWindow(v) {
    console.log(v);
    if (v === null) {
        var now = new Date();
        v = [ getISOWeekYear(now), getISOWeek(now)]
    }
    var d = setISOWeek(new Date(v[0], 0, 1), v[1]);
    
    return [ 
        getUnixTime(startOfISOWeek(d)) * 1000,
        getUnixTime(endOfISOWeek(d) * 1000)
    ];
}
