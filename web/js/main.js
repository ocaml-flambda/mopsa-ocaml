var editor;
var files = [];
var current_file_id = -1;
var file_counter = 0;

var templates = {
    "c": "int main() {\n\t\n\treturn 0;\n}",
    "python": "def main():\n\t\n\treturn\n\nmain()"
}

require.config({ paths: { 'vs': 'monaco-editor/min/vs' }});
require(['vs/editor/editor.main'], function() {
    editor = monaco.editor.create(document.getElementById('editor'), {
	minimap: { enabled: false },
        scrollBeyondLastLine: false,
	glyphMargin: true
    });

    editor.onDidChangeModelContent(function(e) {
	if (current_file_id == -1)
            return;
	file = files.find(file => file.id == current_file_id);
        file.content = editor.getValue();
    });

    add_file("hello.c");
    add_file("hello.py");
    select_file_by_name("hello.c");

});

function get_file_language(filename) {
    ext = filename.split('.').pop();
    switch (ext) {
    case "c": return "c";
    case "py": return "python";
    case "java": return "java";
    default: return "";
    }
}

function get_language_icon(lang) {
    switch (lang) {
    case "c":      return "far fa-copyright";
    case "python": return "fab fa-python";
    case "java":   return "fab fa-java";
    default:       return "";
    }
}

function add_file(filename) {
    lang = get_file_language(filename);
    icon = get_language_icon(lang);
    $("#files-panel").append(`
<a class="list-group-item list-group-item-action p-2 flex-column file-entry" href="#" id="file${file_counter}">
  <div class="d-flex w-100 justify-content-between">
    <div><i class="${icon}"></i> ${filename}</div>
    <div class="status"></div>
  </div>
</a>
`);
    $("#newFileName").val('');
    files.push({
	id:file_counter,
	name: filename,
	lang: lang,
	content: templates[lang],
	decorations: [],
        decorations_ids: []
    });
    file_counter = file_counter + 1;
    select_file_by_id(file_counter - 1);
    $("#removeFileButton").prop("disabled", false);
    $("#analyzeButton").prop("disabled", false);
}

function remove_selected_file() {
    selected = $("#files-panel").find('.active');
    selected.remove();
    i = files.findIndex(file => file.id == current_file_id);
    n = files.length;
    files.splice(i, 1);
    if (files.length == 0) {
        current_file_id = -1;
        editor.setValue("");
        $("#removeFileButton").prop("disabled", false);
	$("#analyzeButton").prop("disabled", false);
    } else {
        if (i == n - 1) {
            i = i - 1;
        }
        current_file_id = files[i].id;
        select_file_by_id(current_file_id);
    }
}

function analyze_selected_file() {
    file = files.find(file => file.id == current_file_id);

    $("#files-panel").find('.active .status').html(`<span class="badge badge-pill badge-info">
                                                   Please wait<span class="dotdotdot"></span>
                                                   </span>`);
    $.post("analyze.php",
           { name: file.name, lang: file.lang, content: file.content},
           function (data) {
               if (data.success == false) {
                   exception = data.exception;
                   $("#files-panel").find('.active .status').html(`<span class="badge badge-pill badge-danger" title="${exception}">
                                                                  &nbsp;failure&nbsp;
                                                                  </span>`);
	           file.decorations_ids = editor.deltaDecorations(file.decorations_ids, []);
	           file.decorations = [];
               } else {
	           if (data.alarms.length == 0) {
	               $("#files-panel").find('.active .status').html('<span class="badge badge-pill badge-success">&nbsp;safe&nbsp;</span>');
	               file.decorations_ids = editor.deltaDecorations(file.decorations_ids, []);
	               file.decorations = [];
	           } else {
	               $("#files-panel").find('.active .status').html(`<span class="badge badge-pill badge-warning">
                                                                      &nbsp;${data.alarms.length} alarm${data.alarms.length > 1 ? 's' : ''}&nbsp;
                                                                      </span>`);
	               file.decorations = data.alarms.map(a => {
		           line = a.range.split(":")[1];
		           return {
		               range: new monaco.Range(line, 1, line, 1),
		               options: {
			           hoverMessage: {value: a.title},
			           glyphMarginHoverMessage: {value: a.title},
			           isWholeLine: true,
			           className: 'alarm-line',
			           glyphMarginClassName: 'fas fa-exclamation-triangle text-danger'
		               }
		           }
	               });
	               file.decorations_ids = editor.deltaDecorations(file.decorations_ids, file.decorations);
	           }
               }
           });
}

function select_file(file) {
    old_id = current_file_id;

    if (old_id != -1) {
	$("#file"+old_id).removeClass("active");
    }

    current_file_id = -1;
    editor.setValue(file.content);
    monaco.editor.setModelLanguage(editor.getModel(), get_file_language(file.name));

    if (old_id != -1) {
        old_file = files.find(file => file.id == old_id);
        file.decorations_ids = editor.deltaDecorations([], file.decorations);
    }

    current_file_id = file.id;
    $("#file"+file.id).addClass("active");
}

function select_file_by_name(name) {
    file = files.find(file => file.name == name);
    select_file(file);
}

function select_file_by_id(id) {
    file = files.find(file => file.id == id);
    select_file(file);
}

function parse_file_id(e) {
    var re = /file(\d+)/;
    id = e.attr('id').match(re)[1];
    return id;
}

$("#files-panel").on("click", ".file-entry", function(e) {
    id = parse_file_id($(this));
    select_file_by_id(id);
});

$("#newFileButton").on('click', function(e) {
    $("#newFileModal").modal("show");
});

$("#newFileCreate").on('click', function(e) {
    add_file($("#newFileName").val());
    $("#newFileModal").modal("hide");
});

$("#newFileName").on('keyup', function(e) {
    var key = 'which' in e ? e.which : e.keyCode;
    if (key == 13) {
	add_file($("#newFileName").val());
	$("#newFileModal").modal("hide");
    }
});

$("#removeFileButton").on('click', function(e) {
    remove_selected_file();
    if (current_file_id == -1) {
        $("#removeFileButton").prop("disabled", true);
	$("#analyzeButton").prop("disabled", true);
    }
});

$('#newFileModal').on('shown.bs.modal', function() {
    $('#newFileName').focus();
});

$("#analyzeButton").on('click', function(e) {
    analyze_selected_file();
});
