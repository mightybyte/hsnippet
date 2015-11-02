<apply template="base">

  <ifLoggedIn>
    <div id="hsnippet-app" class="full-height">
    </div>

    <script src="/rts.js"></script>
    <script src="/lib.js"></script>
    <script src="/hsnippet.js"></script>

    <ignore>
    <apply template="mockup"/>
    </ignore>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
