var dict = {"-1": "&#10060;", "0": "&#10067;", "1": "&#9989;"}

function generateTestTable()
{
  var machine_list=[];
  var machine_revision_list=[];
  var machine_full_name_list=[];
  var machine_info_list=[];

  if(typeof test_jeanzay_revision_list != 'undefined') 
  {
    machine_list.push("jeanzay");
    machine_revision_list.push("test_jeanzay_revision_list")
    machine_full_name_list.push("Jean-Zay")
    machine_info_list.push("test_jeanzay_info_list")
  }
  
  if (typeof test_irene_revision_list != 'undefined') 
  {
    machine_list.push("irene");
    machine_revision_list.push("test_irene_revision_list")
    machine_full_name_list.push("Irene")
    machine_info_list.push("test_irene_info_list")
  }
  
  if (typeof test_other_revision_list != 'undefined') 
  {
    machine_list.push("other");
    machine_revision_list.push("test_other_revision_list")
    machine_full_name_list.push("Other")
    machine_info_list.push("test_other_info_list")
  }

  var revision_list=[]

  for (var i=0; i<machine_list.length; i++)
  {
    tmp_list = eval(machine_revision_list[i])

    for (var j=0; j<tmp_list.length; j++)
    {
      if (revision_list.includes(tmp_list[j]) == false)
      {
        revision_list.push(tmp_list[j])
      }
    }
  }

  revision_list.sort(function(a, b){return b-a});

  // Generate select list for test table

  var sel = document.getElementById("revision");

  var sel_options = sel.children
  
  var new_options=[]
  for(var i=0; i<sel_options.length-1; i++)
  {
    new_options[i] = sel_options[i+1].value
  }
  
  for(var i=0; i<revision_list.length; i++)
  {
    if (!new_options.includes(revision_list[i]))
    {
      new_options.push(revision_list[i])
    }
  }
  
  new_options.sort(function(a, b){return b-a})
  
  while (sel.lastChild && sel.children.length>1) {
    sel.removeChild(sel.lastChild);
  }

  for(var i=0; i<new_options.length; i++)
  {
    var opt = document.createElement('option');
    opt.appendChild( document.createTextNode(new_options[i]));
    opt.value = new_options[i]; 
    sel.appendChild(opt);
  }
  


  
  // Generate content of test table
  
  var table = document.getElementById("test_table")
  table.style.backgroundColor = "#84c5ff";
  table.setAttribute("class", "test_table")
  table.setAttribute("id", "test_table")

  var titelRow = document.getElementById("test_table_row0")
  var myCell = document.createElement("TD");
  myCell.style.minWidth="150px"
  myCell.style.maxWidth="150px"
  myCell.innerHTML = ("Revision");
  titelRow.appendChild(myCell)

  for (var machine_index=0; machine_index<machine_list.length; machine_index++)
  {
    var myCell = document.createElement("TD");
    myCell.style.minWidth="50px"
    myCell.innerHTML = (machine_full_name_list[machine_index]);
    titelRow.appendChild(myCell)
  }

  var emptyRow = document.createElement("tr")
  emptyRow.setAttribute("id", "empty_test_row")
  emptyRow.style.display="none"
  table.appendChild(emptyRow)
  
  var myCell = document.createElement("TD");
  myCell.setAttribute("id", "empty_test_row_revision")
  myCell.style.fontWeight="bold"
  emptyRow.appendChild(myCell)

  for (var machine_index=0; machine_index<machine_list.length; machine_index++)
  {
    var myCell = document.createElement("TD");
    myCell.innerHTML="No Test Information"
    myCell.style.color="white"
    emptyRow.appendChild(myCell)
  }


  for (var i=0; i<revision_list.length; i++)
  {
    var revision = revision_list[i]

    var myRow = document.createElement("TR");
    myRow.setAttribute("id", "test_table_"+revision);
    myRow.setAttribute("class", "test_table_row");
    table.appendChild(myRow)
    
    var myCell = document.createElement("TD");
    myCell.innerHTML = (revision);
    myCell.style.fontWeight = "bold";
    myRow.appendChild(myCell)
    
    for( var j=0; j<machine_list.length; j++)
    {
      var machine = machine_list[j]
      var myCell = document.createElement("TD");
      myCell.setAttribute("id", "test_table_"+revision+"_"+machine);
      myRow.appendChild(myCell)
      myRow.appendChild(myCell)
    }        
  }

  for (var i=0; i<revision_list.length; i++)
  {
    var revision = revision_list[i]
    for (var j=0; j<machine_list.length; j++)
    {
      var machine = machine_list[j]
      var has_test_info=false
      tmp_info = eval(machine_info_list[j])
      for (var k=0; k<eval(machine_info_list[j]).length; k++)
      {
        target_info = tmp_info[k]
        if (target_info[0] == revision && target_info[1] == machine)
        {
          fillSubTestTable(revision, machine, target_info[2], target_info[3], target_info[4], target_info[5], target_info[6], target_info[7])
          has_test_info = true
        }
      }
      if (!has_test_info)
      {
        var myTD = document.getElementById("test_table_"+revision+"_"+machine);
        myTD.innerHTML = "No Test Information"
        myTD.style.color = "white"
      }
    }
  }
  update_status()
}

function fillSubTestTable(revision, machine, build_dir, branch_name, machine_name, arch_name, mode_name, full_dir)
{
  var info_list = eval("test_"+machine+"_"+revision+"_"+build_dir.replace("build_",""))

  var myTD = document.getElementById("test_table_"+revision+"_"+machine);
  var mySubTable = document.createElement("table")
  myTD.appendChild(mySubTable)
  mySubTable.style.minWidth="200px"
  mySubTable.setAttribute("class", "test_sub_table")
  mySubTable.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir)

  if(info_list.length == 0)
  {
    myRow = document.createElement("tr")
    myRow.setAttribute("id","test_sub_table_row_"+revision+"_"+machine+"_"+build_dir)
    myRow.setAttribute("class","test_sub_table_row_"+revision+"_"+machine)
    myRow.classList.add("build_level")
    myCell = document.createElement("td")
    myCell.innerHTML = build_dir.replace("build_","")+" Build Failed"
    myCell.style.color = "white"
    myCell.style.fontWeight = "bold"
    myRow.appendChild(myCell)
    mySubTable.append(myRow)
    return;
  }

  myRow = document.createElement("tr")
  myRow.setAttribute("id","test_sub_table_row_"+revision+"_"+machine+"_"+build_dir)
  myRow.setAttribute("class","test_sub_table_row_"+revision+"_"+machine)
  myRow.classList.add("build_level")
  myCell = document.createElement("td")
  myCell.innerHTML = build_dir.replace("build_","")
  myCell.onclick=function() {mytoggle("test_sub_table_"+revision+"_"+machine+"_"+build_dir)}
  myRow.appendChild(myCell)
  mySubTable.append(myRow)

  myCell = document.createElement("td")
  myCell_1 = document.createElement("div")
  myCell_1.setAttribute("class", "dropdown")
  myCell_2 = document.createElement("button")
  myCell_2.onclick = function() {show_dropdown("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_dropdown")}
  myCell_2.onmouseleave = function() {hide_dropdown("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_dropdown")}
  myCell_2.innerHTML=("&#128270;")
  myCell_3 = document.createElement("div")
  myCell_3.setAttribute("class", "dropdown-content")
  myCell_3.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_dropdown")
  myCell_4 = document.createElement("a")
  myCell_4.innerHTML = ("show plain report")
  myCell_4.onclick = function() {show_plain_report(machine, revision, build_dir)}
  myCell_5 = document.createElement("a")
  myCell_5.innerHTML = ("show xios info")
  myCell_5.onclick = function() {show_text("<table><tr><td>Branch</td><td><em>"+branch_name+"</em></td></tr><tr><td>Revision</td><td><em>"+revision+"</em></td></tr><tr><td>Machine</td><td><em>"+machine_name+"</em></td></tr><tr><td>Architecture</td><td><em>"+arch_name+"</em></td></tr><tr><td>Compile mode</td><td><em>"+mode_name+"</em></td></tr><tr><td>Full build directory</td><td><em>"+full_dir+"</em></td></tr><tr><td>Short build directory</td><td><em>"+build_dir+"</em></td></tr></table>")}
  myCell_2.appendChild(myCell_3)
  myCell_3.appendChild(myCell_4)
  myCell_3.appendChild(myCell_5)
  myCell_1.appendChild(myCell_2)
  myCell.appendChild(myCell_1)
  myRow.appendChild(myCell)

  for (var i=0; i<2; i++)
  {
    myCell = document.createElement("td")
    myRow.appendChild(myCell)
  }

  myCell = document.createElement("td")
  myCell.setAttribute("class", "cell_to_update")
  myCell.setAttribute("id","test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_status")
  myCell.innerHTML = 1
  myRow.appendChild(myCell)

  for (var i=0; i<3; i++)
  {
    myCell = document.createElement("td")
    myRow.appendChild(myCell)
  }

  var previous_status=["","","",""]
  var current_status=info_list[0]
  for(var i=0; i<info_list.length; i++)
  {
    current_status = info_list[i]
    if (current_status[0] != previous_status[0])
    {
      create_rows(3, revision, machine, build_dir, current_status, full_dir)
    }
    else if (current_status[1] != previous_status[1])
    {
      create_rows(2, revision, machine, build_dir, current_status, full_dir)
    }
    else if (current_status[2] != previous_status[2])
    {
      create_rows(1, revision, machine, build_dir, current_status, full_dir)
    }
    previous_status = current_status
  }


}


function create_rows(nb, revision, machine, build_dir, status, full_dir)
{
  tt = document.getElementById("test_sub_table_"+revision+"_"+machine+"_"+build_dir)
  if(nb==3)
  {
    // algo level

    myRow = document.createElement("tr")
    myRow.setAttribute("class", "test_sub_table_"+revision+"_"+machine+"_"+build_dir)
    myRow.classList.add('algo_level');
    myRow.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0])
    myRow.style.display = "none";
    tt.appendChild(myRow)
    
    
    myCell = document.createElement("td")
    myRow.appendChild(myCell)


    myCell = document.createElement("td")
    myCell.innerHTML = status[0]
    myCell.onclick=function() {mytoggle("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0])}
    myRow.appendChild(myCell)

    myCell = document.createElement("td")
    myCell_1 = document.createElement("div")
    myCell_1.setAttribute("class", "dropdown")
    myCell_2 = document.createElement("button")
    myCell_2.onclick = function() {show_dropdown("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_dropdown")}
    myCell_2.onmouseleave = function() {hide_dropdown("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_dropdown")}
    myCell_2.innerHTML=("&#128270;")
    myCell_3 = document.createElement("div")
    myCell_3.setAttribute("class", "dropdown-content")
    myCell_3.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_dropdown")
    myCell_4 = document.createElement("a")
    myCell_4.innerHTML = ("show user_params.def")
    myCell_4.onclick = function() {show_user_params(machine, revision, build_dir, status[0])}
    myCell_5 = document.createElement("a")
    myCell_5.innerHTML = ("show location")
    myCell_5.onclick = function() {show_text(full_dir.replace(build_dir, "")+status[0])}
    myCell_2.appendChild(myCell_3)
    myCell_3.appendChild(myCell_4)
    myCell_3.appendChild(myCell_5)
    myCell_1.appendChild(myCell_2)
    myCell.appendChild(myCell_1)
    myRow.appendChild(myCell)

    for (var i=0; i<2; i++)
    {
      myCell = document.createElement("td")
      myRow.appendChild(myCell)
    }

    myCell = document.createElement("td")
    myCell.innerHTML = 1
    myCell.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_status")
    myCell.setAttribute("class", "cell_to_update")
    myRow.appendChild(myCell)
    
    for (var i=0; i<2; i++)
    {
      myCell = document.createElement("td")
      myRow.appendChild(myCell)
    }
  }

  if(nb>=2)
  {
    // config level

    myRow = document.createElement("tr")
    myRow.setAttribute("class", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0])
    myRow.classList.add("config_level")
    myRow.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1])
    myRow.style.display = "none";
    tt.appendChild(myRow)
    
    for (var i=0; i<2; i++)
    {
      myCell = document.createElement("td")
      myRow.appendChild(myCell)
    }

    myCell = document.createElement("td")
    myCell.innerHTML = status[1]
    myCell.onclick=function() {mytoggle("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1])}
    myRow.appendChild(myCell)

    myCell = document.createElement("td")
    myCell_1 = document.createElement("div")
    myCell_1.setAttribute("class", "dropdown")
    myCell_2 = document.createElement("button")
    myCell_2.onclick = function() {show_dropdown("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1]+"_dropdown")}
    myCell_2.onmouseleave = function() {hide_dropdown("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1]+"_dropdown")}
    myCell_2.innerHTML=("&#128270;")
    myCell_3 = document.createElement("div")
    myCell_3.setAttribute("class", "dropdown-content")
    myCell_3.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1]+"_dropdown")
    myCell_4 = document.createElement("a")
    myCell_4.innerHTML = ("show all_param.def")
    myCell_4.onclick = function() {show_all_params(machine, revision, build_dir, status[0], status[1])}
    myCell_5 = document.createElement("a")
    myCell_5.innerHTML = ("show location")
    myCell_5.onclick = function() {show_text(full_dir.replace(build_dir, "")+status[0]+"/"+status[1])}
    myCell_2.appendChild(myCell_3)
    myCell_3.appendChild(myCell_4)
    myCell_3.appendChild(myCell_5)
    myCell_1.appendChild(myCell_2)
    myCell.appendChild(myCell_1)
    myRow.appendChild(myCell)


    for (var i=0; i<2; i++)
    {
      myCell = document.createElement("td")
      myRow.appendChild(myCell)
    }

    myCell = document.createElement("td")
    myCell.innerHTML = 1
    myCell.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1]+"_status")   
    myCell.setAttribute("class", "cell_to_update")
    myRow.appendChild(myCell)
    
    myCell = document.createElement("td")
    myRow.appendChild(myCell)
  }

  // file level

  myRow = document.createElement("tr")
  myRow.setAttribute("class", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1])
  myRow.classList.add("file_level")
  myRow.setAttribute("id", "test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1]+"_"+status[2])
  myRow.style.display = "none";
  tt.appendChild(myRow)
    
  for (var i=0; i<3; i++)
  {
    myCell = document.createElement("td")
    myRow.appendChild(myCell)
  }

  myCell = document.createElement("td")
  myCell.innerHTML = status[2]
  myRow.appendChild(myCell)

  for (var i=0; i<3; i++)
  {
    myCell = document.createElement("td")
    myRow.appendChild(myCell)
  }

  myCell = document.createElement("td")
  myCell.setAttribute("class", "cell_to_update")
  myCell.innerHTML = status[3]
  myRow.appendChild(myCell)

  var cell = document.getElementById("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_"+status[1]+"_status")
  cell.innerHTML = Math.min(cell.innerHTML, status[3])
    
  var cell = document.getElementById("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_"+status[0]+"_status")
  cell.innerHTML = Math.min(cell.innerHTML, status[3])

  var cell = document.getElementById("test_sub_table_"+revision+"_"+machine+"_"+build_dir+"_status")
  cell.innerHTML = Math.min(cell.innerHTML, status[3])

}

function update_status()
{
  var cells = document.getElementsByClassName("cell_to_update")
  for (var i=0; i<cells.length; i++)
  {
    cells[i].innerHTML = dict[cells[i].innerHTML]
  }
}

function mytoggle(classname)
{
  var myCells = document.getElementsByClassName(classname)
  for (var i=0; i<myCells.length; i++)
  {
    var mySubCells = document.getElementsByClassName(myCells[i].id)
    if (myCells[i].style.display=="table-row")
    {
      myCells[i].style.display="none" 
      for(var j=0; j<mySubCells.length; j++)
      {
        mySubCells[j].style.display="none"
        var mySubSubCells = document.getElementsByClassName(mySubCells[j].id)
        for(var k=0; k<mySubSubCells.length; k++)
        {
          mySubSubCells[k].style.display="none"
        }
      }
    }
    else
    {
      myCells[i].style.display="table-row"
    }
  }
}

