import axios from 'axios';

// const options = {
//     method: 'POST',
//     url: 'http://localhost:3000/parse',
//     headers: {'Content-Type': 'application/json', 'User-Agent': 'insomnia/8.5.1'},
//     data: {source: '(1,(2,3))'}
// };

// axios.request(options).then(function (response) {
//         console.log(response.data);
//     }).catch(function (error) {
//         console.error(error);
//     });

function populateTree({ conclusion, premises }) {
    return {
        conclusionSource: conclusion,
        premises: premises.map(p => populateTree(p)),
        shown: false,
        selected: false,
    };
}

export async function getProofTree(source, url = 'localhost', port = 3000) {
    const options = {
        method: 'POST',
        url: `http://localhost:3000/parse`,
        headers: {
            'Content-Type': 'application/json',
            // 'Access-Control-Allow-Origin': 'http://localhost:8080',
            // 'Access-Control-Allow-Origin': 'http://localhost:3000',
        },
        data: {source: source}
    };
    console.log(source);
    const response = await axios.request(options);
    console.log(response.data);
    let tree = populateTree(response.data);
    tree.shown = true;
    tree.selected = true;
    for (const premise of tree.premises) {
        premise.shown = true;
    }
    return tree;
}