<style type="text/css">
  body {
    background-color: #DADADA;
  }
  body > .grid {
    height: 100%;
  }
  .image {
    margin-top: -100px;
  }
  .column {
    max-width: 450px;
  }
</style>

<div class="ui middle aligned center aligned grid">
  <div class="column">
    <h2 class="ui teal image header">
      <div class="content">
        Register a new user
      </div>
    </h2>

    <bind tag="postAction">/new_user</bind>
    <bind tag="submitText">Add User</bind>
    <apply template="userform"/>

    <div class="ui message">
      Don't have a login yet?  <a href="/new_user">Sign Up</a>
    </div>
  </div>
</div>