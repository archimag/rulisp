$(function () {
    var jqconsole = $('#console').jqconsole('JSCL еще далек от завершения, поддержите проект на: github.com/davazp/jscl!\n\n', '');

    $('.jqconsole').css('position', '');
    $('.jqconsole').css('height', '500px');

    jqconsole.RegisterMatching('(', ')', 'parents');

    if (localStorage.getItem('jqhist'))
        jqconsole.SetHistory(JSON.parse(localStorage.getItem('jqhist')));

    lisp.write = function(str){
        jqconsole.Write(xstring(str), 'jqconsole-output', false);
        return str;
    };

    var startPrompt = function () {
        // Start the prompt with history enabled.
        jqconsole.Write(lisp.evalString('(CL:PACKAGE-NAME CL:*PACKAGE*)') + '> ', 'jqconsole-prompt');
        jqconsole.Prompt(true, function (input) {
            // Output input with the class jqconsole-return.
            if (input[0] != ','){
                try {
                    var vs = lisp.evalInput(input);
                    jqconsole.Write(lisp.print(vs) + '\n\n', 'jqconsole-return');
                    localStorage.setItem('jqhist', JSON.stringify(jqconsole.GetHistory()));
                } catch(error) {
                    var msg = error.message || error || 'Unknown error';
                    if (typeof(msg) != 'string') msg = xstring(msg);
                    jqconsole.Write('ERROR: ' + msg + '\n\n', 'jqconsole-error');
                }
            } else
                jqconsole.Write(lisp.compileString(input.slice(1)) + '\n\n', 'jqconsole-return');

            // Restart the prompt.
            startPrompt();
        }, function(input){
            try {
                lisp.read(input[0]==','? input.slice(1): input);
            } catch(error) {
                return 0;
            }
            return false;
        });
    };
    startPrompt();
});
