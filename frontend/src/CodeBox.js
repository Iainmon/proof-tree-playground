import CodeMirror from '@uiw/react-codemirror';
import { StreamLanguage } from '@codemirror/language';
import { haskell } from './haskell_ext_lang.js';
// import { haskell } from '@codemirror/legacy-modes/mode/haskell';

export function CodeBox({ value, onChange, language}) {
    return (
        <CodeMirror
            value={value}
            onChange={onChange}
            theme="dark"
            height="200px"
            extensions={[StreamLanguage.define(haskell)]}
            // options={{
            //     theme: 'material',
            //     lineNumbers: true,
            //     mode: 'go'
            // }}
        />
    )
}