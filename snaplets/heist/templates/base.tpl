<!DOCTYPE html>
<html lang="en">
  <head>
    <apply template="_page_head"/>
  </head>

  <body class="${bodyClasses}">
    <ifLoggedIn>
      <apply template="_admin_nav"/>
    </ifLoggedIn>

    <div id="body-inner">

      <header id="site-header">
        <nav id="site-navigation" class="navbar navbar-default">
          <div class="container-fluid">
            <div class="navbar-header">
              <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
                <span class="sr-only">Toggle navigation</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
              </button>
              <a class="navbar-brand" href="/">Jim Pinfold <small>[prototype]</small></a>
            </div>

            <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
              <ul class="nav navbar-nav">
                <li><a href="/">Index</a></li>
              </ul>
              <ul class="nav navbar-nav navbar-right">
                <ifLoggedOut>
                  <li>
                    <a href="/login">Login</a>
                  </li>
                </ifLoggedOut>
              </ul>
            </div><!-- /.navbar-collapse -->
          </div>
        </nav>
      </header>

      <header id="page-header" class="container">
        <div class="page-header">
          <ifBound tag="pageActions">
            <div id="page-actions" class="pull-right">
              <ul class="nav nav-pills">
                <pageActions/>
              </ul>
            </div>
          </ifBound>

          <pageHeader/>
        </div>
      </header>

      <div id="content" class="container">
        <div class="row">
          <div class="col-sm-12">
            <main/>
          </div>
        </div>
      </div>

      <div id="footer" class="container">
        <footer/>
      </div>
    </div>
  </body>
</html>
