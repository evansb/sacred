
Session.setDefault("mode", "edit");
Session.setDefault("language", "javascript");

Template.FileControl.helpers({
    languages: function () {
        return [
            { lang: "javascript" }
        ];
    },
    ext: function () {
        return ".js";
    }
});

Template.NewCode.helpers({
    editorOptions : {
        mode: Session.get("language"),
        lineNumbers: true
    }
});

Template.ViewCode.helpers({
    editorOptions : {
        mode: Session.get("language"),
        lineNumbers: true,
        readOnly: true
    }
});

