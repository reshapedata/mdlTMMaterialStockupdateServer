#' 预览
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回测试数据集
#' @export
#'
#' @examples
#' materialstock_update_previewServer()
materialstock_update_previewServer <- function(input,output,session,dms_token) {

  file_materialstock_update = tsui::var_file('file_materialstock_update')

  shiny::observeEvent(input$btn_materialstock_update_view,{
    filename = file_materialstock_update()
    data <- readxl::read_excel(filename)
    data = as.data.frame(data)
    data = tsdo::na_standard(data)
    tsui::run_dataTable2(id ='dt_materialstock_update_res' ,data =data )

  })
}

#' 更新数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回测试数据集
#' @export
#'
#' @examples
#' materialstock_update_uploadServer()
materialstock_update_uploadServer <- function(input,output,session,dms_token) {

  file_materialstock_update = tsui::var_file('file_materialstock_update')

  shiny::observeEvent(input$btn_materialstock_update_update,{
    dms_token=dms_token
    filename = file_materialstock_update()
    data <- readxl::read_excel(filename,col_types = c("text","text", "numeric", "numeric",
                                                      "numeric","text","text","text"))

    data = as.data.frame(data)
    data = tsdo::na_standard(data)

    tsda::db_writeTable2(token = dms_token,table_name = 'rds_erp_src_t_MATERIALSTOCK_update',r_object = data,append = TRUE)
    mdlTMMaterialStockupdatePkg::materialstock_tm_bak(dms_token =dms_token )
    mdlTMMaterialStockupdatePkg::materialstock_tm_update(dms_token = dms_token)
    mdlTMMaterialStockupdatePkg::materialstock_tm_delete(dms_token = dms_token)
    tsui::pop_notice("更新完成")

  })
}

#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' materialstock_updateServer()
materialstock_updateServer <- function(input,output,session,dms_token) {


  mdlTMMaterialStockupdateServer::materialstock_update_previewServer(input = input,output =output ,session = session,dms_token =dms_token )

  mdlTMMaterialStockupdateServer::materialstock_update_uploadServer(input = input,output =output ,session = session,dms_token =dms_token )





}
