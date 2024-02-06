import katex from 'katex';
import { Katex } from './katex.js';


export function MyButton() {
    return (
        <button>I'm a button</button>
    );
}

function Conclusion({ conclusionSource }) {
    return (
        <div className="proof-tree-conclusion" dangerouslySetInnerHTML={{ __html: katex.renderToString(conclusionSource) }}>
            {/* <Katex source={conclusionSource} /> */}
        </div>
    );
}

export function Node({ tree: {conclusionSource, premises, shown, selected, hidden } }) {
    if (!shown) {
        return null;
    }

    const spacedPremisesNodes = [];
    for (let i = 0; i < premises.length; i++) {

        if (premises[i].shown === false) continue;
        const premiseNode = <Node key={i} tree={premises[i]} />;
        spacedPremisesNodes.push(premiseNode);
        if (i < premises.length - 1) {
            spacedPremisesNodes.push(<div className="proof-tree-premises-spacer" key={i + 's'} />);
        }
    }
    if (spacedPremisesNodes.length === 0 || hidden === true) {
        return (
            <div className="proof-tree-node">
                <div className={'proof-tree-conclusion' + (selected === true ? ' proof-tree-selected-node' : '')}>
                    <Conclusion conclusionSource={hidden === true ? '\\ldots ': conclusionSource} />
                </div>
            </div>
        );
    }
    return (
        <div className="proof-tree-node">
            <div className="proof-tree-premises" >
                {spacedPremisesNodes.map(n => n)}
            </div>
            <div className={'proof-tree-conclusion' + (selected === true ? ' proof-tree-selected-node' : '')}>
                <Conclusion conclusionSource={hidden === true ? '\\ldots ': conclusionSource} />
            </div>
        </div>
    );
}



