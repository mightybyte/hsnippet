<apply template="base">

  <ifLoggedIn>
    <div id="hsnippet-app">
      <p>Congrats!  You're logged in as '<loggedInUser/>'</p>
    </div>

    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
