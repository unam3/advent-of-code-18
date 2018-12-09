/*
--- Part Two ---

Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)
*/

const test = {
    ids: [
        'abcde',
        'fghij',
        'klmno',
        'pqrst',
        'fguij',
        'axcye',
        'wvxyz'
    ],
    commonLetters: 'fgij'
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

const getIds = (ids) =>
    ids.filter(
        (id) => {

            const {hasTwoTimeLetter, hasThreeTimeLetter} = getLettersCount(id);

            if (hasTwoTimeLetter || hasThreeTimeLetter) {

                return true;

            }
        }
    );

const commonLettersIfOneDiff = (id_a, id_b) => {

    let diffLettersCount = 0;

    let i = 0;

    const commonLetters = [];

    while (id_a[i] && diffLettersCount < 2) {
        
        if (id_a[i] !== id_b[i]) {
        
            diffLettersCount += 1;

        } else {
            
            commonLetters.push(id_a[i]);

        }

        i += 1;

    }
    
    return (diffLettersCount === 1) && commonLetters.join('');
};

const getCommonLetters = (ids) => {
    
    const hasAlreadyCheckedPair = ids.reduce(
        (acc, id) => {
            acc[id] = {};

            return acc;
        },
        {}
    );

    //console.log(hasAlreadyCheckedPair);

    for (let [i, id_a] of ids.entries()) {

        if (i + 1 !== ids.length) {

            for (let [j, id_b] of ids.entries()) {

                if (i !== j && !hasAlreadyCheckedPair[id_b][id_a]) {

                    hasAlreadyCheckedPair[id_a][id_b] = true;
                
                    //console.log(i, id_a, j, id_b, hasAlreadyCheckedPair);

                    //console.log(i, id_a, j, id_b, commonLettersIfOneDiff(id_a, id_b));

                    const commonLetters = commonLettersIfOneDiff(id_a, id_b);

                    if (commonLetters) {

                        return commonLetters;

                    }

                }
            
            }

        }
    }
};

//console.log(getCommonLetters(test.ids));

const testResults = getCommonLetters(test.ids) === test.commonLetters;

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

const result = getCommonLetters(getIds(input.split(/\n/g)));

// cypueihajytordkgzxfqplbwn
console.log('puzzle answer is', result);
