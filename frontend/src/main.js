import { putKatex } from './katex.js';
import './main.css';

import { dragElement } from './drag.js';
import { Node } from './PTNode.js';
import { createRoot } from 'react-dom/client';
import { useState, useReducer } from 'react';



let proofTree = {
    conclusionSource: '\\Gamma \\vdash (1,(2,3)) : Z \\times (Z \\times Z)',
    shown: true,
    selected: true,
    premises: [
        {
            conclusionSource: '\\Gamma \\vdash 1 : Z',
            shown: false,
            selected: false,
            premises: [
                {
                    conclusionSource: '1 \\in Z',
                    shown: false,
                    selected: false,
                    premises: []
                },
            ]
        },
        {
            conclusionSource: '\\Gamma \\vdash (2,3) : Z \\times Z',
            shown: false,
            selected: false,
            premises: [
                {
                    conclusionSource: '\\Gamma \\vdash 2 : Z',
                    shown: false,
                    selected: false,
                    premises: [
                        {
                            conclusionSource: '2 \\in Z',
                            shown: false,
                            selected: false,
                            premises: []
                        },
                    ]
                },
                {
                    conclusionSource: '\\Gamma \\vdash 3 : Z',
                    shown: false,
                    selected: false,
                    premises: [
                        {
                            conclusionSource: '3 \\in Z',
                            shown: false,
                            selected: false,
                            premises: []
                        },
                    ]
                },
            ]
        },
    ]
}

function getTree(tree,path) {
    const [idx, ...rest] = path;
    return getTreeHelper(tree,rest);
}

function getTreeHelper(tree,path) {
    if (path.length === 0) return tree;
    const [idx, ...rest] = path;
    return getTreeHelper(tree.premises[idx],rest);
}

window.getTree = getTree;

function updateTree(tree,path) {
    const [idx, ...rest] = path;

}

let eventHandlers = {
    keyUpdate: null
};



function Handler() {
    const [tree, setTree] = useState(proofTree);
    const [path, setPath] = useState([0]);
    const [, forceUpdate] = useReducer(x => x + 1, 0);


    eventHandlers.keyUpdate = key => {
        let newPath = path;

        let selectedNode = getTree(tree,path);
        selectedNode.selected = false;



        if (key === 'down') {
            newPath.pop();
        } else if (key === 'up') {
            if (selectedNode.premises.length > 0) {
                newPath.push(0);
                for (const premise of selectedNode.premises) {
                    premise.shown = true;
                }
            }
        } else if (key === 'left') {
            const lastIdx = newPath.length - 1;
            newPath[lastIdx] = Math.max(0,newPath[lastIdx] - 1);
        } else if (key === 'right') {
            const lastIdx = newPath.length - 1;
            const parentPath = path.slice(0,lastIdx);
            const parentNode = getTree(tree,parentPath);
            
            const maxIdx = Math.max(0,parentNode.premises.length - 1);
            newPath[lastIdx] = Math.min(maxIdx,newPath[lastIdx] + 1);
        } else {
            return;
        }


        let newSelectedNode = getTree(tree,newPath);
        newSelectedNode.selected = true;
        


        console.log(newPath);
        console.log(proofTree);

        setPath(newPath);
        setTree(tree);
        forceUpdate();
    }

    for (const premise of tree.premises) {
        premise.shown = true;
    }

    return (
        <Node tree={tree} />
    );
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
    reactRoot.render(<Handler />);
}

function keyPress(event) {
    const keyMap = {
        37: 'left',
        38: 'up',
        39: 'right',
        40: 'down',
    };

    const key = keyMap[event.keyCode];

    if (eventHandlers.keyUpdate !== null) {
        eventHandlers.keyUpdate(key);
    }
    
}

window.onkeyup = keyPress;


window.main = main;