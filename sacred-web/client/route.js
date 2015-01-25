
Router.route('/', function() {
    this.render('NewCode');
});

Router.route('/new', function() {
    this.render('NewCode');
});

Router.route('/submission', function() {
    this.render('ViewCode');
});


Router.route('/submission/:id', function() {
    this.render('ViewCode');
});

Router.route('/review', function() {
    this.render('ReviewCode');
});

Router.route('/review/:id', function() {
    this.render('ReviewCode');
});
