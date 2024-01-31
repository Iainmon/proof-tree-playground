import React from 'react';
import ReactDOM from 'react-dom';
import { useState, useReducer, useRef } from 'react';
import { createRoot } from 'react-dom/client';

import Button from 'react-bootstrap/Button';
import { Container } from 'react-bootstrap';
import Form from 'react-bootstrap/Form';

import { putKatex } from './katex.js';
// import { dragElement } from './drag.js';
import { Node } from './PTNode.js';


import { getProofTree } from './network.js';

import './App.scss';



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



let eventHandlers = {
    keyUpdate: null
};



function Handler() {
    const [tree, setTree] = useState(proofTree);
    const [path, setPath] = useState([0]);
    const [, forceUpdate] = useReducer(x => x + 1, 0);
    const [x, setX] = useState(0);
    const [y, setY] = useState(0);


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

    function handleNewTree(tree) {
        setTree(tree);
        setPath([0]);
    }

    window.addEventListener("wheel", e => {

        setY(e.deltaY + y);
        setX(e.deltaX + x);

        forceUpdate();
        e.preventDefault();


    }, { passive: false });

    return (
        <>
            <div className="proof-tree" style={{
                transform: 'translate('+ -x + 'px, ' + -y + 'px) scale(1)'
            }}>
                <Node tree={tree} />
            </div>
            <hr />
            <SourceInput handleNewTree={handleNewTree} />
        </>
    );
}

function SourceInput({ handleNewTree }) {
    const [source, setSource] = useState('let x = Just 1 in let y = Nothing in case x of { Just z -> Just y ; Nothing -> 0 }');
    
    async function handleClick() {
        const tree = await getProofTree(source);
        handleNewTree(tree);
    }

    function onChange(event) {
        setSource(event.target.value);
    }


    return (
        <>
            <label>
                Expression: 
                <Form.Control type="text" value={source} onChange={onChange} />
                <Button variant="info" onClick={handleClick}>Submit</Button>
            </label>
        </>
    );
}

function App() {
    return (
        <Container className="p-3">
            <Container className="pb-1 p-5 mb-4 rounded-3">
                <Handler />
            </Container>
        </Container>
    )
}

async function main() {


    const root = document.getElementById('doc-root');
    // dragElement(root);

    // const conclusionTargets = document.querySelectorAll('.proof-tree-conclusion');
    // for (const conclusion of conclusionTargets) {

    //     const ks = '\\Gamma \\vdash x : \\tau';
    //     putKatex(ks, conclusion);
    // }

    const reactRoot = createRoot(root);
    // reactRoot.render(<MyButton />);
    reactRoot.render(<App />);
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
    console.log(event.keyCode);
    
}

document.addEventListener('keydown',keyPress);


window.main = main;