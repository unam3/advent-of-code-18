/*
--- Day 2: Inventory Management System ---

You stop falling through time, catch your breath, and check the screen on the device. "Destination reached. Current Year: 1518. Current Location: North Pole Utility Closet 83N10." You made it! Now, to find those anomalies.

Outside the utility closet, you hear footsteps and a voice. "...I'm not sure either. But now that so many people have chimneys, maybe he could sneak in that way?" Another voice responds, "Actually, we've been working on a new kind of suit that would let him fit through tight spaces like that. But, I heard that a few days ago, they lost the prototype fabric, the design plans, everything! Nobody on the team can even seem to remember important details of the project!"

"Wouldn't they have had enough fabric to fill several boxes in the warehouse? They'd be stored together, so the box IDs should be similar. Too bad it would take forever to search the warehouse for two similar box IDs..." They walk too far away to hear any more.

Late at night, you sneak to the warehouse - who knows what kinds of paradoxes you could cause if you were discovered - and use your fancy wrist device to quickly scan every box and produce a list of the likely candidates (your puzzle input).

To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID containing exactly two of any letter and then separately counting those with exactly three of any letter. You can multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.

For example, if you see the following box IDs:

abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab contains three a and three b, but it only counts once.
Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.

What is the checksum for your list of box IDs?
*/

const test = {
    ids: [
        {id: 'abcdef', hasTwoTimeLetter: false, hasThreeTimeLetter: false},
        {id: 'bababc', hasTwoTimeLetter: true,  hasThreeTimeLetter: true},
        {id: 'abbcde', hasTwoTimeLetter: true,  hasThreeTimeLetter: false},
        {id: 'abcccd', hasTwoTimeLetter: false, hasThreeTimeLetter: true},
        {id: 'aabcdd', hasTwoTimeLetter: true,  hasThreeTimeLetter: false},
        {id: 'abcdee', hasTwoTimeLetter: true,  hasThreeTimeLetter: false},
        {id: 'ababab', hasTwoTimeLetter: false, hasThreeTimeLetter: true}
    ],
    checksum: 12
};


const getLettersCount = (id) => {
    
    let hasTwoTimeLetter, hasThreeTimeLetter;

    const checkedLetters = {};

    let countOfLetter;

    Array.prototype.forEach.call(
        id,
        (letter) => {

            if (checkedLetters[letter]) {
                return;
            }

            checkedLetters[letter] = true;

            countOfLetter = id.split(letter).length - 1;

            if (countOfLetter === 2 && !hasTwoTimeLetter) {

                hasTwoTimeLetter = true;

            } else if (countOfLetter === 3 && !hasThreeTimeLetter) {
            
                hasThreeTimeLetter = true;

            }

            //console.log(id, letter, countOfLetter, checkedLetters);
        }
    );

    //console.log(hasTwoTimeLetter, hasThreeTimeLetter);

    return {hasTwoTimeLetter, hasThreeTimeLetter};
};

const getChecksum = (ids) => {
    const lettersCount = ids.reduce(
        (acc, id) => {

            //console.log(acc, id);

            const {hasTwoTimeLetter, hasThreeTimeLetter} = getLettersCount(id);

            if (hasTwoTimeLetter) {
                
                acc.twoTimeLettersCount += 1;

            }

            if (hasThreeTimeLetter) {
                
                acc.threeTimeLettersCount += 1;

            }

            return acc;
        },
        {twoTimeLettersCount: 0, threeTimeLettersCount: 0}
    );

    return lettersCount.twoTimeLettersCount * lettersCount.threeTimeLettersCount;
};

const testResults = getChecksum(
    test.ids.map((obj) => obj.id)
) === test.checksum;

console.log('test', testResults ? 'passed' : 'failed');


const input = `cvfueihajytpmrdkgsxfqplbxn
cbzueihajytnmrdkgtxfqplbwn
cvzucihajytomrdkgstfqplqwn
cvzueilajytomrdkgsxfqwnbwn
cvzueihajytomrdkgsgwqphbwn
wuzuerhajytomrdkgsxfqplbwn
cyzueifajybomrdkgsxfqplbwn
cvzueihajxtomrdkgpxfqplmwn
ivzfevhajytomrdkgsxfqplbwn
cvzueihajytomrdlgsxfqphbbn
uvzueihajjtomrdkgsxfqpobwn
cvzupihajytomrdkgsxfqplpwe
cvzueihajyvomrdkgsxfqplbrl
cczueihajytomrdkgsnfqpxbwn
cvzueigajytdmrdkgsxyqplbwn
cvzujihljytomrdkgsxuqplbwn
cvzueisajytomrddgsxkqplbwn
cvzneihajytomrdkgsgaqplbwn
cvzueihajytomrdkgsinmplbwn
cveueihajyromrdkgsxfqplown
cypueihajytotrdkgzxfqplbwn
cvzuoihajytomvdqgsxfqplbwn
cvzuekhejytwmrdkgsxfqplbwn
cvzseihajytomrdkgsxfqgmbwn
cvfuhihajytomrdkgsxfqplbwi
cvzueihujxtomrdkgsufqplbwn
cvzueihdjytomrdogsxfqplbwh
cvzueihdjyfohrdkgsxfqplbwn
cvtudihajytolrdkgsxfqplbwn
cvzueihajytymrdkgshzqplbwn
cvzuebhajytomxdkgsxfqplbwt
cvzulihajyxomrdkgsbfqplbwn
cvzueihajywomrdkgsxfqplbts
cvzueihajytouodkdsxfqplbwn
cvzueihajytomgdkgqxfqklbwn
cvzubihajytomvdkgsxfqplmwn
cvhueihajyyocrdkgsxfqplbwn
zvzueihajytourdkgsxflplbwn
cvzbeihajytomadkgsxfoplbwn
cvzueihajytomrdkgnxfqplbsl
cvfueihajftkmrdkgsxfqplbwn
cvzuexhajytomryugsxfqplbwn
cvzueihajytomsckgsxfqalbwn
cvzuexhajytomrdkbsxfqpluwn
cvzueihajytbmrtkgsxwqplbwn
cvzueihajytomrdigsxfqqlbsn
cvzweihajytomydkgsxfmplbwn
bvzteihajytimrdkgsxfqplbwn
cvzueihajytpmrdkgsxfcpbbwn
cvzueigsjltomrdkgsxfqplbwn
cvzueihajytomrikgsxfopldwn
cvzueihajstomrdkgsxfqplgon
cvzueimajytomrnkxsxfqplbwn
cvzleihagatomrdkgsxfqplbwn
cvbueihajotomrdkgsxfqjlbwn
cvzueihajytomrdkgsxfqppnvn
hvzueihajytomrdkghxfkplbwn
cvzueigajytxmrdkgsxfqplbjn
cvzueihaaxtokrdkgsxfqplbwn
cvzueihajyeomrdkgujfqplbwn
cvzueiwajpoomrdkgsxfqplbwn
cvzieidtjytomrdkgsxfqplbwn
cvzueihalytomrakbsxfqplbwn
wtzueihajytomrdkgsxfqplbwq
cvzuelhaiytomrdkgsxfqplcwn
cvzueihajytomrdkgsxfqslswd
cvzueihajytomrykgssfqplbon
cvzueihfjytovrdegsxfqplbwn
cvzueihajytomldqgsxfqplbwy
cvzleihjjytomrtkgsxfqplbwn
cvzueihaldtomrdtgsxfqplbwn
cvzueihajytzmrdkgsxfeplqwn
cvzueihrjytomddkgsxfqpgbwn
cyzulihajytokrdkgsxfqplbwn
cvsueihajytoordfgsxfqplbwn
fvzueyhajytomrdkgaxfqplbwn
cczueihajytobrdkgsefqplbwn
cvzueihajytomcdrgscfqplbwn
cvzuexhajyvomrdkgssfqplbwn
cvzsmihajyiomrdkgsxfqplbwn
cvzzeihajttomrdkgsxzqplbwn
cvzseihajytomrdkgsxfqpebvn
cvzueihajgthmrdkgsbfqplbwn
ruzueihajytomrdkgsxfqphbwn
cvzueihajytofrdkgsnfrplbwn
cvzuetdajytojrdkgsxfqplbwn
fvzueihajytomrdkghxfqpobwn
cvzueihsjytomrdkgsxfqglbxn
cvzueihajytowrdkgsxfqpsbun
cvzteihaiytomrdkfsxfqplbwn
cvzueihajytkmrdkrsxfqplvwn
cvzueihajyoomrdkasxfqjlbwn
lvzurihajytkmrdkgsxfqplbwn
cvzueihajyyomrdagsxfqelbwn
cvfueihajytomrdkgsxfqplbbx
cvwueihajytommdkgkxfqplbwn
cvzucicajytomrdkgsxcqplbwn
dvzueihahytgmrdkgsxfqplbwn
cvzuechajytomrdkgsxfqelwwn
cvzuekhajytomrdkgsxknplbwn
cvtueihajytomphkgsxfqplbwn
cvzueihabytnzrdkgsxfqplbwn
cvzusihajytomrdkgfxfqplban
cvfueihajytomcdfgsxfqplbwn
mvzueihapytomrdkgsxfdplbwn
cvzueihajytomhdkgsxmqppbwn
jvsueihajytomrdkgsxfqplbln
cvzujihajybomrdkgsxtqplbwn
cvzuekhawytomrdkgsxfqplbwc
svzueihanytomrdogsxfqplbwn
cvzujihajytodrdkgslfqplbwn
cvgdeihajytorrdkgsxfqplbwn
cvzbeihajytoprdkgsxfqplbyn
cvzueihkyytomjdkgsxfqplbwn
cvzuelhojytomrdkgsxfqjlbwn
evzueihajytimrdkgsxfqpsbwn
cvzueihajydomrdkjsxfqplbjn
ovzteihajytosrdkgsxfqplbwn
cvzueihajyaomrdzgsxfqplbgn
cvzuewhajmtomrdkgsufqplbwn
cvzueihajqtomhukgsxfqplbwn
cvzueihajytomzqkgsxfqplbwk
cazuewhakytomrdkgsxfqplbwn
clzueihatytomrdkgzxfqplbwn
dvzueihajytomqdkgsxfqpnbwn
cvzueidajdtomrdkgsxtqplbwn
cvzueihabytowrdkgsxoqplbwn
cvzujihwjytomrdkgsxeqplbwn
cvtuedhajytomrdkgsxfqplbbn
cvzueihajcgomrdkgsxfqplswn
cvzuephajyiomrdngsxfqplbwn
cvzueihajythmqdkgsxfqplbwf
cvzueitajytomrdkgsxfepvbwn
cvzueihajytomydkgsxfqplvwb
dvzueshajytomrddgsxfqplbwn
cvzueihajytomrdkgvxfqpwben
cvzueihajytomrdkgvxfpplwwn
cvzuefhajftomrdkgsxfqrlbwn
cvzueihajytpmrvkgsxfqplbcn
cvzueihajytohrdkgsxfqxnbwn
cvzueihajytomrdposxfqulbwn
cozueihajytomrpkgsxfqrlbwn
cvzuuihaxytomrdkgsxfqplbtn
cvzueihajytomrbzgsxyqplbwn
cveueihajyxoqrdkgsxfqplbwn
cvzueihajytomrkkgsxfqptbrn
cvzuezhajatomrdkssxfqplbwn
cpzueihajytomrdkgsxfhplbwo
lviueihajytomrekgsxfqplbwn
cvzueihwjytomrdkusxfyplbwn
cvzgeihajytomwdkgsxfrplbwn
cvzsejhzjytomrdkgsxfqplbwn
cvzuuihajytomrdkgsxfqdlbwz
cvzjeihajytomrdugsxftplbwn
cvzueihaxytomrrkgsxfmplbwn
cvzueihajgtomrdhgsxfqplwwn
cvzulihajytomedkgsxfqplewn
cvzueivajytomrdkmsxfqplbwc
cvzuervajytomrdkgsxfwplbwn
cvzuemhcjytomrdkgslfqplbwn
cvzyerhauytomrdkgsxfqplbwn
cvzueihaoytomrdkgsyfqplewn
cvzueihanytomrdkgsafkplbwn
cvzueihajvtomrdugsxfqpcbwn
chzueihajytamrdxgsxfqplbwn
cvzueihalytomrdsgsxfqplbln
cvzueihajytoyaykgsxfqplbwn
tlzueihajyeomrdkgsxfqplbwn
cvpueihajytbmrdkgsxfxplbwn
cvzueihajytomjdkgsxuqplkwn
cvzueihajygomrdkgkxfqplbwg
cvzueihajhtomrdkgbxsqplbwn
cvzurihajytomrdkgsafqplbwx
cdzuezhajytomrdkgsxrqplbwn
cvbueihajytotrwkgsxfqplbwn
cwzkeihajytomrdkgsxfqplbwh
cvzheihajytolrikgsxfqplbwn
cozuevhajytomrdkgkxfqplbwn
chzueihajytomrjkgsxfqulbwn
cvzueihkjyromrdkgsxvqplbwn
cvzveihajytomrdkgsxpqplnwn
cvzueihajytoirdkgsxfqihbwn
cvoueihajytomrdkgsxfqpdawn
pvzueihajytomrdkgnxfqplbfn
cvzueihakytomxdkgssfqplbwn
cvzueivajytomrdbgsxaqplbwn
cvzueihajytokrdkgszrqplbwn
cvzuevhajytomrdkgsxgqplbwi
cvzueihajylomrdkgsxflplbpn
hvzueihajytomvdkgsxfqplgwn
cvzleihajytymrrkgsxfqplbwn
crzueieajytomrdkgsxfqplbon
cszueihajytomrdlgqxfqplbwn
cvzueihacytomrdkgsxfjblbwn
cvzreihajytomrdkgsxfqplzun
cvzurihajytomrdkgsxiqplawn
uvzueihajyhovrdkgsxfqplbwn
cvzueihajyqodrdkgssfqplbwn
cvzwiihrjytomrdkgsxfqplbwn
cqzueihajytomrdkgjxfqplban
cvmueihajytoordkgsxfqplbyn
cypueihajytomrdkgzxfqplbwn
cvzueihajykomrdkgsmfqplbtn
cvzueidajytimrdkgsxfqpdbwn
cvzheihajytomrdkgsxfqpfewn
dvzueihajytumrdzgsxfqplbwn
cvzueixajytomrdkgsvfqplgwn
cvzuevhzjyzomrdkgsxfqplbwn
cvyeeihajytomrdkgsxnqplbwn
cvzueihajytomrdkggtpqplbwn
cvzceiyajytomrdkgexfqplbwn
cvzuelhajyyomrdkzsxfqplbwn
cvzhzihajygomrdkgsxfqplbwn
cvzueihwjytomrdkgsgfqplbrn
cvzsevhajytomrdkgqxfqplbwn
cvzueiuajytomrdkgsxfppebwn
nvzueihajytemrdkgsxwqplbwn
cvzueihajytocgdkgsxfqvlbwn
cczusihajytomrdkgsxfqplbpn
cmzueihajytomrdkbsxwqplbwn
cvzumfdajytomrdkgsxfqplbwn
cvzueihcjytomrdkgsxfqplbkl
cvzueihajytomawknsxfqplbwn
kvzueihijytomrdkgsxdqplbwn
cdzutihajytomrdkgsxfkplbwn
cvzufihadylomrdkgsxfqplbwn
cvzueihajytomrgkxsxfqphbwn
cvzuewhajyzomrdkgsxfqelbwn
cvzueihajytomrdkgqxfqelbwc
cvzueshajyoomrdkgsxfqflbwn
cvzueihajyromrekgixfqplbwn
chzugihajytomrdkgsxfqplawn
cvzueihajytomrdkgsxfhpmbwy
cvzueihacytodxdkgsxfqplbwn
cvzurihajytourdkgsdfqplbwn
cvzzeihmjytomrddgsxfqplbwn
cvzucyhajygomrdkgsxfqplbwn
ckzueihzjytomrdkgsxwqplbwn
cvlueihajmtozrdkgsxfqplbwn
cvzkeihajytomrdkgsxfqclbwc
cvzueihajytomrdkgsxgdplbwa
cvzueihyjytoxrdkgcxfqplbwn
cvzueizavytomfdkgsxfqplbwn
cvzueihajwtosrdkgsxfqllbwn
cvzueihajytomrdaksxfqpllwn
cvzuuihojytombdkgsxfqplbwn
cvzuiibajytpmrdkgsxfqplbwn
cvzueihajyuomydkgsxfqplzwn
cvzueihajytimrmkgsxfqplfwn
cvzueihajytomrdkgzxfqpljwo`;

const result = getChecksum(input.split(/\n/g));

// 5166
console.log('result is', result);
