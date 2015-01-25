
var Code = new Mongo.Collection("code");

Session.setDefault("mode", "edit");
Session.setDefault("language", "javascript");

Router.route('/', function () {
    this.render('NewCode');
});

Router.route('/new', function () {
    this.render('NewCode');
});

Router.route('/submission', function () {
    this.render('ViewCode');
});

Router.route('/review/:id', function () {
    var foo = Code.findOne({ _id : Router.current().params.id});
    this.render('ReviewCode', {
        data: {
            editorOptions: {
                mode: Session.get("language"),
                readOnly: true
            },
            editorCode: foo.content
        }
    });
});

Template.NewCode.helpers({
    languages: function () {
        return [
            { lang: "javascript" }
        ];
    },
    editorOptions : {
        mode: Session.get("language"),
        lineNumbers: true
    }
});

Template.NewCode.events({
    "click #submit_btn": function(event, template) {
        var filename = template.$("#filename").val();
        var content = Template.instance().find("#editor").value;
        Code.insert({
            filename: filename,
            content : content,
            createdAt: new Date() // current time
        });
        Router.go('/submission');
        return false;
    }
});

Template.ViewCode.helpers({
    codes: function() {
        return Code.find({});
    },
    editorOptions : {
        mode: Session.get("language"),
        lineNumbers: true,
        readOnly: true
    }
});

