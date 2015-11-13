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
    <ul id="project-index">
      <projects>
        <li>
          <h4><projectTitle /></h4>
          <p>
            <projectDescription /> <br/>
          </p>
          <ul class="admin-actions">
            <li>
              <a href="/projects/edit/${projectId}">Edit</a>
            </li>
            <li>
              <a href="/projects/delete?id=${projectId}"
                 data-confirm="Are you sure you want to delete ${projectTitle}?"
              >Delete</a>
            </li>
          </ul>
        </li>
      </projects>
    </ul>
  </bind>
</apply>
