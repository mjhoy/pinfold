<apply template="base">
  <bind tag="subtitle"> | Content</bind>

  <bind tag="pageHeader">
    <h1>Content administration</h1>
  </bind>

  <bind tag="pageActions">
    <li>
      <a href="/projects/new">+ Create project</a>
    </li>
  </bind>

  <bind tag="main">
    <projects>
      <div class="project">
        <h4><projectTitle /></h4>
        <p>
          <projectDescription />
        </p>
        <div class="btn-group" role="group">
          <a class="btn btn-default" href="/projects/edit/${projectId}">Edit</a>
          <a class="btn btn-default" href="/projects/delete?id=${projectId}"
             data-confirm="Are you sure you want to delete ${projectTitle}?"
          >Delete</a>
        </div>
      </div>
    </projects>
  </bind>
</apply>
