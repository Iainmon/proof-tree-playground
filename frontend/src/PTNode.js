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

export function Node({ tree: {conclusionSource, premises} }) {

    const premiseNodes = premises.map((premise, i) => <Node key={i} tree={premise} />);
    const spacedPremisesNodes = [];
    for (let i = 0; i < premiseNodes.length; i++) {
        spacedPremisesNodes.push(premiseNodes[i]);
        if (i < premiseNodes.length - 1) {
            spacedPremisesNodes.push(<div className="proof-tree-premises-spacer" key={i + 's'} />);
        }
    }
    return (
        <div className="proof-tree-node">
            <div className="proof-tree-premises">
                {spacedPremisesNodes.map(n => n)}
            </div>
            <div className="proof-tree-conclusion">
                <Conclusion conclusionSource={conclusionSource} />
            </div>
        </div>
    )
}



