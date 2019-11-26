package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 02/07/16.
 */

case class DeleteDocumentResult(index: String,
                                id: String,
                                version: Long,
                                found: Boolean
                               )

case class DeleteDocumentsResult(data: List[DeleteDocumentResult])

case class DeleteDocumentsSummaryResult(message: String, deleted: Long)
