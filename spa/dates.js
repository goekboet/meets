var getUnixTime = require('date-fns/getUnixTime')
var setISOWeek = require('date-fns/setISOWeek')
var getISOWeeksInYear = require('date-fns/getISOWeeksInYear')
var startOfISOWeek = require('date-fns/startOfISOWeek')
var endOfISOWeek = require('date-fns/endOfISOWeek')
var getISOWeekYear = require('date-fns/getISOWeekYear')
var getISOWeek = require('date-fns/getISOWeek')
var format = require('date-fns/format')
var fromUnixTime = require('date-fns/fromUnixTime')

function parseweek(s) {
    var tokens = s === null ? [] : s.split('-');
    // console.log(tokens);
    if (tokens.length == 2) {
        var y = parseInt(tokens[0], 10);
        var w = parseInt(tokens[1], 10);

        if (isNaN(y) || isNaN(w))
        {
            // console.log("failed to parse year: " + y + " week " + w)
            return null;
        }

        if (w < 1 && getISOWeeksInYear(new Date(y, 0)) > w)
        {
            // console.log("Reject week: " + w);
            return null;
        }

        return [y, w];
    }
    else {
        return null;
    }
}

function zeroPad2w(s) {
    return ("0" + s).slice(-2);
}

function toWeek(d) {
    var y = getISOWeekYear(d);
    var w = getISOWeek(d);

    return y.toString().concat('-', zeroPad2w(w));
}

window.initWeekpointer = function initWeekpointer() {
    var urlParams = new URLSearchParams(window.location.search);
    var week = urlParams.get('week');

    return getWeekpointer(week);
}

window.getWeekpointer = function getWeekpointer(week) {

    var prev = null;
    var curr = null;
    var next = null;
    var query = parseweek(week);
    
    if (query !== null) {
        prev = new setISOWeek(new Date(query[0], 0, 1), query[1] - 1);
        curr = new setISOWeek(new Date(query[0], 0, 1), query[1]);
        next = new setISOWeek(new Date(query[0], 0, 1), query[1] + 1);
    }
    else {
        var now = new Date();
        var wn = getISOWeek(now);
        
        prev = new setISOWeek(now, wn - 1);
        curr = now;
        next = new setISOWeek(now, wn + 1);
    }
    
    // console.log("prev: " + prev);
    // console.log("curr: " + curr);
    // console.log("next: " + next);

    return {
        prev: toWeek(prev),
        curr: toWeek(curr),
        next: toWeek(next),
        window: [ getUnixTime(startOfISOWeek(curr)) * 1000,
                  getUnixTime(endOfISOWeek(curr) * 1000)]

    }
}

function oclock(t) {
    return format(fromUnixTime(t), 'PPpp');

}

window.printOclock = function printOclock(appts)
{
     return appts.map(function f(a) {
        return {
            hostId: a.hostId,
            name: a.name,
            start: a.start,
            oclock: oclock(a.start / 1000),
            dur: a.dur
        };
     })
}
