<apply template="base">
  <bind tag="subtitle"> | New project</bind>

  <bind tag="pageHeader">
    <h1>Projects</h1>
  </bind>

  <bind tag="main">
    <ul>
      <projects>
        <li>
          <p>
            <b><projectTitle /></b>
          </p>
          <p>
            <projectDescription />
          </p>
        </li>
      </projects>
    </ul>
  </bind>
</apply>
