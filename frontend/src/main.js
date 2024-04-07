import React from 'react';
import ReactDOM from 'react-dom';
import { useState, useReducer, useRef } from 'react';
import useLocalStorage from 'use-local-storage';
import { createRoot } from 'react-dom/client';

import Button from 'react-bootstrap/Button';
import Alert from 'react-bootstrap/Alert';
import { Container } from 'react-bootstrap';
import Form from 'react-bootstrap/Form';

import { putKatex } from './katex.js';
// import { dragElement } from './drag.js';
import { Node } from './PTNode.js';


import { getProofTree } from './network.js';

import { CodeBox } from './CodeBox.js';

import './App.scss';

let proofTree = {
    conclusionSource: '\\rho \\vdash e \\Rightarrow v',
    shown: true,
    selected: true,
    elided: false,
    flagged: false,
    premises: []
}

let proofTree_ = {
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
    // const [storeTree, setStoreTree] = useLocalStorage('proofTree', proofTree, {syncData: false, logger: console.log});
    // console.log('Old Tree', oldTree === proofTree);

    // console.log(oldTree);

    const [tree, setTree] = useLocalStorage('proofTree', proofTree, {syncData: false, logger: console.log});// useState({...storeTree});
    const setStoreTree = setTree;
    // const [oldTree,setStoreTree] = useLocalStorage('proofTree', tree);

    const [path, setPath] = useLocalStorage('path', [0], {syncData: false, logger: console.log});
    const [, forceUpdate] = useReducer(x => x + 1, 0);
    const [x, setX] = useState(0);
    const [y, setY] = useState(0);


    eventHandlers.keyUpdate = key => {
        let newPath = path;

        let selectedNode = getTree(tree,path);
        selectedNode.selected = false;

        // setStoreTree(tree);



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
        } else if (key === 'h') {
            selectedNode.hidden = !selectedNode.hidden;
        } else if (key === 'e') {
            selectedNode.elided = !selectedNode.elided;
        } else if (key === 's') {
            // const t = {...tree};
            setStoreTree({...tree});
            console.log('Tree stored', tree);
        } else if (key === 'f') {
            selectedNode.flagged = !selectedNode.flagged;
        } else {
            // return;
        }


        let newSelectedNode = getTree(tree,newPath);
        newSelectedNode.selected = true;
        


        console.log(newPath);

        setPath([...newPath]);
        setStoreTree({...tree});

        // setTree(tree);
        forceUpdate();
    }
    // if (oldTree === proofTree) {
    //     for (const premise of tree.premises) {
    //         premise.shown = true;
    //     }
    // }


    function handleNewTree(tree) {
        setStoreTree({...tree});
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
            <div className="fixed-bottom">
                <SourceInput handleNewTree={handleNewTree} />
            </div>
        </>
    );
}

function SourceInput({ handleNewTree }) {
    const [source, setSource] = useLocalStorage('expression','let x = Just 1 in let y = Nothing in case x of { Just z -> Just y ; Nothing -> 0 }');
    const [errorMessages, setErrorMessages] = useState('');
    async function handleClick() {
        try {
            const tree = await getProofTree(source);
            handleNewTree(tree);
            setErrorMessages('');
        } catch (e) {
            console.log(e.response.status,e.response.data);
            setErrorMessages(e.response.data.error);
        }
    }

    function onChange(text) {
        setSource(text);
        // setSource(event.target.value);
    }


    return (
        <>
            <Alert variant="danger" show={errorMessages !== ''} onClose={() => setErrorMessages('')} dismissible>
                {errorMessages}
            </Alert>
            <CodeBox value={source} onChange={onChange} />
            <Button variant="info" onClick={handleClick}>Submit</Button>
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
        72: 'h',
        69: 'e',
        83: 's',
        70: 'f',
    };

    const key = keyMap[event.keyCode];

    if (eventHandlers.keyUpdate !== null) {
        eventHandlers.keyUpdate(key);
    }
    console.log(event.keyCode);
    
}

document.addEventListener('keydown',keyPress);


window.main = main;