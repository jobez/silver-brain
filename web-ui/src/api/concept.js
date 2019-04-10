const axios = require('axios')

export async function getAllConcepts () {
  let response = await axios.get('/concepts')
  return response.data
}

export async function newConcept (name, content, contentFormat) {
  let data = {
    name: name,
    content: content,
    contentFormat: contentFormat
  }
  let response = await axios.post('/concepts', data)
  if (response.status !== 201) {
    throw Error('Failed to add new concept')
  }
  return response.headers.location.substr(10)
}

export async function searchConcept (search) {
  let response = await axios.get(`/concepts?search=${search}`)
  return response.data
}

/**
 * Return Array<Object{uuid, name}>
 */
export async function conceptParents (uuid) {
  let response = await axios.get(`/concepts/${uuid}/parents`)
  return response.data
}

export async function getConceptByUuid (uuid) {
  let response = await axios.get(`/concepts/${uuid}`)
  return response.data
}