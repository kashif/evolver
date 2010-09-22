// reorder.cmd

define vertex attribute vertex_order_key real
define edge attribute edge_order_key real
define facet attribute facet_order_key real
define body attribute body_order_key real
define facetedge attribute facetedge_order_key real

reorder := {
  set vertex vertex_order_key x+y+z;
  set edge ee edge_order_key min(ee.vertex,vertex_order_key);
  set facetedge fe facetedge_order_key fe.edge[1].edge_order_key;
  set facet ff facet_order_key min(ff.vertex,vertex_order_key);
  set body bb body_order_key min(bb.facet,facet_order_key);
  reorder_storage;
  }
