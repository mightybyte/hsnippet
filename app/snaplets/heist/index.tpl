<apply template="base">

  <ifLoggedIn>
    <div id="hsnippet-app" style="height:100%">
    </div>

    <script src="/hsnippet.js"></script>

    <ignore>
    <apply template="mockup"/>
    </ignore>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
