import katex from 'katex';
import 'katex/contrib/auto-render';

export function putKatex(ks, target) {
    const source = katex.renderToString(ks);
    target.innerHTML = source;
}


export function Katex({ source }) {
    return (
        <div dangerouslySetInnerHTML={{ __html: katex.renderToString(source) }} />
    )
}

