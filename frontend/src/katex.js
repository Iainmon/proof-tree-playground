import katex from 'katex';
import 'katex/dist/katex.min.css';
import 'katex/dist/contrib/auto-render';

export function putKatex(ks, target) {
    const source = katex.renderToString(ks);
    target.innerHTML = source;
}


export function Katex({ source }) {
    return (
        <div dangerouslySetInnerHTML={{ __html: katex.renderToString(source) }} />
    )
}

