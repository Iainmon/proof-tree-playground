import { putKatex } from './katex.js';
import './main.css';

import { dragElement } from './drag.js';
import { Node } from './PTNode.js';
import { createRoot } from 'react-dom/client';



const proofTree = {
    conclusionSource: '\\Gamma \\vdash (1,(2,3)) : Z \\times (Z \\times Z)',
    premises: [
        {
            conclusionSource: '\\Gamma \\vdash 1 : Z',
            premises: []
        },
        {
            conclusionSource: '\\Gamma \\vdash (2,3) : Z \\times Z',
            premises: [
                {
                    conclusionSource: '\\Gamma \\vdash 2 : Z',
                    premises: []
                },
                {
                    conclusionSource: '\\Gamma \\vdash 3 : Z',
                    premises: []
                },
            ]
        },
    ]
}

async function main() {


    const root = document.getElementById('doc-root');
    dragElement(root);

    // const conclusionTargets = document.querySelectorAll('.proof-tree-conclusion');
    // for (const conclusion of conclusionTargets) {

    //     const ks = '\\Gamma \\vdash x : \\tau';
    //     putKatex(ks, conclusion);
    // }

    const reactRoot = createRoot(root);
    // reactRoot.render(<MyButton />);
    reactRoot.render(<Node tree={proofTree} />);






}

async function keyPress(event) {
    const keyMap = {
        37: 'left',
        38: 'up',
        39: 'right',
        40: 'down',
    };

    const key = keyMap[event.keyCode];

    // if 
}


window.main = main;