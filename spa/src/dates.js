import getISOWeek from 'date-fns/getISOWeek' 
import startOfISOWeek from 'date-fns/startOfISOWeek' 
import setISOWeek from 'date-fns/setISOWeek'
import format from 'date-fns/format'
import getUnixTime from 'date-fns/getUnixTime'
import isThisISOWeek from 'date-fns/isThisISOWeek' 

let week = d => {
    let w = getISOWeek(d)
    let curr = startOfISOWeek(d)
    let prev = startOfISOWeek(setISOWeek(d, w - 1))
    let next = startOfISOWeek(setISOWeek(d, w + 1))

    let toOutput = x => { 
        return { 
            name: format(x, "RRRR 'w.'II"), 
            ts: getUnixTime(x), 
            isNow: isThisISOWeek(x) } 
        }

    return {
        current: toOutput(curr),
        previous: toOutput(prev),
        next: toOutput(next)
    }
}

export { week }