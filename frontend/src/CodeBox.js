import CodeMirror from '@uiw/react-codemirror';
import { StreamLanguage } from '@codemirror/language';
import { haskell } from './haskell_ext_lang.js';
import { hoohui } from './hoohui_ext_lang.js';

// import { haskell } from '@codemirror/legacy-modes/mode/haskell';

const languages = {
    'SimFL': StreamLanguage.define(haskell),
    'Hoohui': StreamLanguage.define(hoohui),
}

export function CodeBox({ value, onChange, language}) {
    return (
        <CodeMirror
            value={value}
            onChange={onChange}
            theme="dark"
            height="320px"
            extensions={[languages[language]]}
            // options={{
            //     theme: 'material',
            //     lineNumbers: true,
            //     mode: 'go'
            // }}
        />
    )
}