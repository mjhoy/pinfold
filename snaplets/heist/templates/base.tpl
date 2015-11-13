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
        <nav id="site-navigation">
          <ul>
            <li>
              <ifLoggedOut>
                <a href="/login">Login</a>
              </ifLoggedOut>
            </li>
          </ul>
        </nav>

        <h1><a href="/">Jim Pinfold</a> [prototype]</h1>
      </header>

      <ifBound tag="pageActions">
        <div id="page-actions">
          <ul>
            <pageActions/>
          </ul>
        </div>
      </ifBound>

      <header id="page-header">
        <pageHeader/>
      </header>

      <div id="content">
        <main/>
      </div>

      <div id="footer">
        <footer/>
      </div>
    </div>
  </body>
</html>
