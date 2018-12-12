/*
For example, consider the following records, which have already been organized into chronological order:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
Timestamps are written using year-month-day hour:minute format. The guard falling asleep or waking up is always the one whose shift most recently started. Because all asleep/awake times are during the midnight hour (00:00 - 00:59), only the minute portion (00 - 59) is relevant for those events.

Visually, these records show that the guards are asleep at these times:

Date   ID   Minute
            000000000011111111112222222222333333333344444444445555555555
            012345678901234567890123456789012345678901234567890123456789
11-01  #10  .....####################.....#########################.....
11-02  #99  ........................................##########..........
11-03  #10  ........................#####...............................
11-04  #99  ....................................##########..............
11-05  #99  .............................................##########.....
The columns are Date, which shows the month-day portion of the relevant day; ID, which shows the guard on duty that day; and Minute, which shows the minutes during which the guard was asleep within the midnight hour. (The Minute column's header shows the minute's ten's digit in the first row and the one's digit in the second row.) Awake is shown as ., and asleep is shown as #.

Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up. For example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.

If you can figure out the guard most likely to be asleep at a specific time, you might be able to trick that guard into working tonight so you can have the best chance of sneaking in. You have two strategies for choosing the best guard/minute combination.

Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?

In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas any other minute the guard was asleep was only seen on one day).

While this example listed the entries in chronological order, your entries are in the order you found them. You'll need to organize them before they can be analyzed.

What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 10 * 24 = 240.)
*/

const test = {
    records: `[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up`,
    firstPuzzlePartReferenceResult: 240,
    secondPuzzlePartReferenceResult: 3
};

const parseRecords = (str) =>
    str.split('\n')
        .map(
            (str) => {

                const [time, stateChange] = str.substring(1).split('] ');

                return {time, stateChange};
            }
        );

//console.log(parseRecords(test.records));

const getMinuteAsNumber = (timeStr) => parseInt(timeStr.split(':')[1]);

const getTimeAsleep = (records) =>

    records.reduce(

        (acc, {time, stateChange}) => {

            if (stateChange.startsWith('Guard #')) {
                
                //'Guard #'.length
                acc.currentGuardId = stateChange.substring(7).split(' begins shift')[0];

                //console.log(acc.currentGuardId);

            } else if (stateChange === 'falls asleep') {
            
                acc.fallsAsleepTime = getMinuteAsNumber(time);

                //console.log(acc.fallsAsleepTime);

            //} else if (stateChange === 'wakes up') {
            } else {

                if (!acc.guardsSleepTime[acc.currentGuardId]) {

                    acc.guardsSleepTime[acc.currentGuardId] = {total: 0, ranges: []};

                }

                const fallsAsleep = acc.fallsAsleepTime;

                const wakesUp = getMinuteAsNumber(time);

                acc.guardsSleepTime[acc.currentGuardId].total += (wakesUp - fallsAsleep);

                acc.guardsSleepTime[acc.currentGuardId].ranges.push([fallsAsleep, wakesUp]);

                //console.log(acc.guardsSleepTime);

                acc.fallsAsleepTime = false;

            }

            //console.log(record, acc);
            
            return acc;
        },
        {currentGuardId: false, fallsAsleep: false, guardsSleepTime: {}}
    );

//console.log(JSON.stringify(getTimeAsleep(parseRecords(test.records)), null, 2));

const getGuardAsleepTheMost = ({guardsSleepTime}) => {
    
    const guardId = Object.entries(guardsSleepTime)
        .sort((a, b) => b[1].total - a[1].total)[0][0];

    return {guardId, ranges: guardsSleepTime[guardId].ranges};

};

//{ guardId: '10', ranges: [ [ 5, 25 ], [ 30, 55 ], [ 24, 29 ] ] }
//console.log(getGuardAsleepTheMost(getTimeAsleep(parseRecords(test.records))));

const getMinuteAsleepTheMost = (ranges) => {

    const asleepMinutes = ranges.reduce(
    
        (acc, range) => {
            
            const [fallsAsleep, wakesUp] = range;
                    
            let currentMinute = fallsAsleep;

            while (currentMinute < wakesUp) {

                if (!acc[currentMinute]) {

                    acc[currentMinute] = 0;

                }

                acc[currentMinute] += 1;

                currentMinute += 1;

            }

            return acc

        },
        {}
    );

    //console.log(asleepMinutes);

    return Object.entries(asleepMinutes).sort((a, b) => b[1] - a[1])[0][0];

};

//console.log(getMinuteAsleepTheMost(getGuardAsleepTheMost(getTimeAsleep(parseRecords(test.records))).ranges));

const getGuardIdByMinute = (input) => {

    const {guardId, ranges} = getGuardAsleepTheMost(getTimeAsleep(parseRecords(test.records)));

    const minute = getMinuteAsleepTheMost(ranges);

    console.log(guardId, minute);

    return parseInt(guardId) * parseInt(minute);

};

const testResults = getGuardIdByMinute(test.records) === test.firstPuzzlePartReferenceResult;

console.log('test', testResults ? 'passed' : 'failed');


//const input = `
//`;

//console.log(
//    getOverlappedSquareInchesList(
//        parseClaims(input)
//    )
//);

//const firstPartPuzzleSolution = getOverlappedSquareInchesCount(
//    getOverlappedSquareInchesList(
//        parseClaims(input)
//    )
//);

//console.log('first part puzzle solution is', firstPartPuzzleSolution);

//const secondPartTestResults = getClaimNumberId(
//    getNonOverlappedClaim(
//        parseClaims(test.claims)
//    )
//) === test.secondPuzzlePartReferenceResult;
//
//console.log('second part test', secondPartTestResults ? 'passed' : 'failed');
//
//const secondPartPuzzleSolution = getClaimNumberId(
//    getNonOverlappedClaim(
//        parseClaims(input)
//    )
//);
//
//console.log('second puzzle part solution', secondPartPuzzleSolution);
