<apply template="base">
  <bind tag="subtitle"> | New project</bind>

  <bind tag="pageHeader">
    <h1>New project</h1>
  </bind>

  <bind tag="main">

    <div class="row">
      <div class="col-sm-6">
        <form id="projectform" method="post" action="/projects">

          <div class="form-group">
            <label for="title">Title</label>

            <input type="text" class="form-control" name="title"/>
          </div>

          <div class="form-group">
            <label for="description">Description</label>
            <input type="text" class="form-control" name="description" size="20" />
          </div>

          <button class="btn btn-primary" type="submit">Create project</button>
          <a class="btn btn-default" href="/content">Cancel</a>
        </form>
      </div>
    </div>

  </bind>
</apply>
