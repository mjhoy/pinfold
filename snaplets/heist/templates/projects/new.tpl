<apply template="base">
  <bind tag="subtitle"> | New project</bind>

  <bind tag="pageHeader">
    <h1>New project</h1>
  </bind>

  <bind tag="main">

    <form id="projectform" method="post" action="/projects">
      <div class="form-item">
        <label for="title">Title:</label>
        <input type="text" name="title" size="20" />
      </div>

      <div class="form-item">
        <label for="description">Description:</label>
        <input type="text" name="description" size="20" />
      </div>

      <div class="form-item">
        <input type="submit" value="Create project" />
      </div>
    </form>

  </bind>
</apply>
